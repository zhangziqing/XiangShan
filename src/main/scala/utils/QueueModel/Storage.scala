// SPDX-License-Identifier: MulanPSL-2.0
package utils.QueueModel

import chisel3._
import chisel3.Data._
import chisel3.util.{Mux1H, PopCount, log2Ceil}

/** Physical-entry access contract, independent of queue membership.
  *
  * A Storage is instantiated inside its owning module; every operation
  * elaborates in that module, so writes observe the current `when` conditions
  * and use the owner's clock and reset. Reads are combinational current-state
  * observations. All entry access is index based.
  */
abstract class Storage[T <: Data](val entries: Int) {
  def read(index: Int): T
  def read(index: UInt): T
  def write(index: UInt, content: T): Unit
  /** Writes one validated static member path of the addressed entry. */
  def writeField[D <: Data](index: UInt, field: FieldOf[T, D], content: D): Unit

  /** Returns one current-state match bit per physical slot, without a hit bound.
    * Validity and query enables belong in the predicate; writes are not forwarded.
    */
  def camHitVec[A](f: (T, A) => Bool, arg: A): Seq[Bool]

  /** Returns `entries` ascending matching indices, padded with the invalid index
    * `entries`. Each unsigned index has width `max(1, log2Ceil(entries + 1))`.
    * Like camHitVec, this is a combinational query of current state.
    */
  def camHitIdx[A](f: (T, A) => Bool, arg: A): Seq[UInt]

  /** Returns `maxHits` (data, hit) pairs in physical index order, padding misses
    * with (zero, false). Asserts if more than `maxHits` entries match.
    */
  def camRead[A](f: (T, A) => Bool, arg: A, maxHits: Int = 1): Seq[(T, Bool)]
  def camWrite[P](f: (T, P) => Bool, param: P, content: T): Unit
  def camWrite[P, D <: Data](
    f: (T, P) => Bool,
    param: P,
    field: FieldOf[T, D],
    content: D
  ): Unit

  def update(f: T => T): Unit

  /** Maps each read-only current entry to one result in physical index order.
    * The result has `entries` elements; pending writes are not forwarded.
    */
  def foreach[A <: Data](f: T => A): Seq[A]
}

/** Register-backed implementation of the Storage contract. */
class RegStorage[T <: Data](
  gen: T,
  entries: Int,
  initContents: Option[Seq[T]] = None
) extends Storage[T](entries) {
  require(entries > 0, "Storage capacity must be positive")

  private val contents: Vec[T] = initContents match {
    case Some(initial) =>
      require(initial.length == entries, "initial contents must describe every storage entry")
      // Preserve the declared entry type; VecInit otherwise narrows scalar UInts
      // to the widest initial literal.
      RegInit(VecInit(initial.map(_.asTypeOf(gen))))
    case None => Reg(Vec(entries, gen))
  }

  override final def read(index: Int): T = {
    require(index >= 0 && index < entries, "static storage index is out of range")
    contents(index).readOnly
  }

  override final def read(index: UInt): T = {
    val selected = (0 until entries).map(slot => index === slot.U)
    val anySelected = selected.reduce(_ || _)
    Mux(anySelected, Mux1H(selected, contents.toSeq), 0.U.asTypeOf(gen)).readOnly
  }

  override final def write(index: UInt, content: T): Unit =
    for (slot <- 0 until entries) {
      when(index === slot.U) { contents(slot) := content }
    }

  override final def writeField[D <: Data](
    index: UInt,
    field: FieldOf[T, D],
    content: D
  ): Unit =
    for (slot <- 0 until entries) {
      when(index === slot.U) { field.select(contents(slot)) := content }
    }

  override final def camHitVec[A](f: (T, A) => Bool, arg: A): Seq[Bool] =
    contents.toSeq.map(entry => f(entry.readOnly, arg).readOnly)

  private def rankedHitMasks(hits: Seq[Bool], count: Int): Seq[Seq[Bool]] = {
    if (count == 1) Seq(hits)
    else {
      val ranks = hits.indices.map(index => PopCount(hits.take(index)))
      (0 until count).map { rank =>
        hits.indices.map(index => hits(index) && ranks(index) === rank.U)
      }
    }
  }

  override final def camHitIdx[A](f: (T, A) => Bool, arg: A): Seq[UInt] = {
    val indexWidth = math.max(1, log2Ceil(entries + 1))
    val indices = (0 until entries).map(_.U(indexWidth.W))
    rankedHitMasks(camHitVec(f, arg), entries).map { mask =>
      val anyHit = mask.reduce(_ || _)
      Mux(anyHit, Mux1H(mask, indices), entries.U(indexWidth.W)).readOnly
    }
  }

  override final def camRead[A](
    f: (T, A) => Bool,
    arg: A,
    maxHits: Int
  ): Seq[(T, Bool)] = {
    require(maxHits > 0 && maxHits <= entries, "camRead maxHits must be between 1 and entries")
    val values = contents.toSeq.map(_.readOnly)
    val hits = camHitVec(f, arg)
    assert(PopCount(hits) <= maxHits.U, "camRead hit count exceeds maxHits")

    rankedHitMasks(hits, maxHits).map { mask =>
      val anyHit = mask.reduce(_ || _)
      (Mux(anyHit, Mux1H(mask, values), 0.U.asTypeOf(gen)).readOnly, anyHit.readOnly)
    }
  }

  override final def camWrite[P](
    f: (T, P) => Bool,
    param: P,
    content: T
  ): Unit =
    for (slot <- 0 until entries) {
      when(f(contents(slot).readOnly, param)) { contents(slot) := content }
    }

  override final def camWrite[P, D <: Data](
    f: (T, P) => Bool,
    param: P,
    field: FieldOf[T, D],
    content: D
  ): Unit =
    for (slot <- 0 until entries) {
      when(f(contents(slot).readOnly, param)) { field.select(contents(slot)) := content }
    }

  override final def update(f: T => T): Unit =
    contents.foreach(entry => entry := f(entry.readOnly))

  override final def foreach[A <: Data](f: T => A): Seq[A] =
    contents.toSeq.map(entry => f(entry.readOnly))
}
