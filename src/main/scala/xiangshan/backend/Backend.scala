package xiangshan.backend

import bus.simplebus.SimpleBusUC
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import noop.MemMMUIO
import xiangshan._
import xiangshan.backend.decode.{DecodeBuffer, DecodeStage}
import xiangshan.backend.rename.Rename
import xiangshan.backend.brq.Brq
import xiangshan.backend.dispatch.{Dispatch1, Dispatch2}
import xiangshan.backend.exu._
import xiangshan.backend.issue.IssueQueue
import xiangshan.backend.regfile.{Regfile, RfWritePort}
import xiangshan.backend.roq.Roq


/** Backend Pipeline:
  * Decode -> Rename -> Dispatch-1 -> Dispatch-2 -> Issue -> Exe
  */
class Backend(implicit val p: XSConfig) extends XSModule
  with HasExeUnits
  with NeedImpl
{
  val io = IO(new Bundle {
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val memMMU = Flipped(new MemMMUIO)
    val frontend = Flipped(new FrontendToBackendIO)
  })


  val decode = Module(new DecodeStage)
  val brq = Module(new Brq)
  val decBuf = Module(new DecodeBuffer)
  val rename = Module(new Rename)
  val dispatch1 = Module(new Dispatch1)
  val roq = Module(new Roq)
  val dispatch2 = Module(new Dispatch2)
  val intRf = Module(new Regfile(
    numReadPorts = NRReadPorts,
    numWirtePorts = NRWritePorts,
    hasZero = true
  ))
  val fpRf = Module(new Regfile(
    numReadPorts = NRReadPorts,
    numWirtePorts = NRWritePorts,
    hasZero = false
  ))
  val redirect = Mux(roq.io.redirect.valid, roq.io.redirect, brq.io.redirect)
  val issueQueues = exeUnits.zipWithIndex.map({ case(eu, i) =>
    def needWakeup(x: Exu): Boolean = (eu.readIntRf && x.writeIntRf) || (eu.readFpRf && x.writeFpRf)
    val wakeupCnt = exeUnits.count(needWakeup)
    val bypassCnt = if(eu.fuTypeInt == FuType.alu.litValue()) exuConfig.AluCnt else 0
    val iq = Module(new IssueQueue(eu.fuTypeInt, wakeupCnt, bypassCnt))
    iq.io.redirect <> redirect
    iq.io.enqCtrl <> dispatch2.io.enqIQCtrl(i)
    iq.io.enqData <> dispatch2.io.enqIQData(i)
    iq.io.wakeUpPorts <> exeUnits.filter(needWakeup).map(_.io.out)
    println(s"[$i] $eu Queue wakeupCnt:$wakeupCnt bypassCnt:$bypassCnt")
    eu.io.in <> iq.io.deq
    eu.io.redirect <> redirect
    iq
  })

  val aluQueues = issueQueues.filter(_.fuTypeInt == FuType.alu.litValue())
  aluQueues.foreach(aluQ => {
    aluQ.io.bypassUops <> aluQueues.map(_.io.selectedUop)
    aluQ.io.bypassData <> aluExeUnits.map(_.io.out)
  })

  lsuExeUnits.foreach(_.io.dmem <> io.dmem)

  io.frontend.redirect <> redirect
  io.frontend.commits <> roq.io.commits

  decode.io.in <> io.frontend.cfVec
  brq.io.roqRedirect <> roq.io.redirect
  brq.io.enqReqs <> decode.io.toBrq
  for(i <- bjUnits.indices){
    brq.io.exuRedirect(i).bits := bjUnits(i).io.out.bits
    brq.io.exuRedirect(i).valid := bjUnits(i).io.out.fire()
  }
  decode.io.brMasks <> brq.io.brMasks
  decode.io.brTags <> brq.io.brTags
  decBuf.io.in <> decode.io.out

  rename.io.redirect <> redirect
  rename.io.roqCommits <> roq.io.commits
  rename.io.in <> decBuf.io.out

  dispatch1.io.redirect <> redirect
  dispatch1.io.in <> rename.io.out
  roq.io.brqRedirect <> brq.io.redirect
  roq.io.dp1Req <> dispatch1.io.toRoq
  dispatch1.io.roqIdxs <> roq.io.roqIdxs

  dispatch2.io.in <> dispatch1.io.out
  dispatch2.io.intPregRdy <> rename.io.intPregRdy
  dispatch2.io.fpPregRdy <> rename.io.fpPregRdy
  intRf.io.readPorts <> dispatch2.io.readIntRf
  rename.io.intRfReadAddr <> dispatch2.io.readIntRf.map(_.addr)
  fpRf.io.readPorts <> dispatch2.io.readFpRf
  rename.io.fpRfReadAddr <> dispatch2.io.readFpRf.map(_.addr)


  val exeWbReqs = exeUnits.map(_.io.out)
  val wbIntReqs = (bruExeUnit +: (aluExeUnits ++ mulExeUnits ++ mduExeUnits)).map(_.io.out)
  val wbFpReqs = (fmacExeUnits ++ fmiscExeUnits ++ fmiscDivSqrtExeUnits).map(_.io.out)
  val intWbArb = Module(new WriteBackArbMtoN(wbIntReqs.length, NRWritePorts))
  val fpWbArb = Module(new WriteBackArbMtoN(wbFpReqs.length, NRWritePorts))
  val wbIntResults = intWbArb.io.out
  val wbFpResults = fpWbArb.io.out

  def exuOutToRfWrite(x: Valid[ExuOutput]) = {
    val rfWrite = Wire(new RfWritePort)
    rfWrite.wen := x.valid
    rfWrite.addr := x.bits.uop.pdest
    rfWrite.data := x.bits.data
    rfWrite
  }

  intWbArb.io.in <> wbIntReqs
  intRf.io.writePorts <> wbIntResults.map(exuOutToRfWrite)

  fpWbArb.io.in <> wbFpReqs
  fpRf.io.writePorts <> wbFpResults.map(exuOutToRfWrite)

  rename.io.wbIntResults <> wbIntResults
  rename.io.wbFpResults <> wbFpResults

  roq.io.exeWbResults.zip(exeWbReqs).foreach({case (x,y) => {
    x.bits := y.bits
    x.valid := y.fire()
  }})


  // TODO: Remove sink and source
  val tmp = WireInit(0.U)
  val sinks = Array[String](
    "DTLBFINISH",
    "DTLBPF",
    "DTLBENABLE",
    "perfCntCondMdcacheLoss",
    "perfCntCondMl2cacheLoss",
    "perfCntCondMdcacheHit",
    "lsuMMIO",
    "perfCntCondMl2cacheHit",
    "perfCntCondMl2cacheReq",
    "mtip",
    "perfCntCondMdcacheReq",
    "meip"
  )
  for (s <- sinks){ BoringUtils.addSink(tmp, s) }

}
