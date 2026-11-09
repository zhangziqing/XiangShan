#!/usr/bin/env python3
# SPDX-License-Identifier: MulanPSL-2.0
"""Replace the VLQ in a legacy-reference miter with its generated SV model.

The Scala miter is the independent behavioral oracle. This driver supplies
identical events to it before and after hierarchical substitution; it contains
no reimplementation of queue next-state logic.
"""
import argparse
import json
from pathlib import Path
import re
import subprocess
import time
from queue_contract import extract, emit_sv, refs


def driver(top_body, depth, scalar_type, vector_type):
    inputs = re.findall(r'^    input (\w+) : (?:UInt<(\d+)>|Clock)', top_body, re.M)
    outputs = dict(re.findall(r'^    output (\w+) : UInt<(\d+)>', top_body, re.M))
    zero = '\n'.join(f'    d.{n} = 0;' for n, w in inputs if n not in ('clock', 'reset', 'io_initDefined'))
    arrays = []
    for prefix in ('needAlloc', 'reqValid', 'reqRobVal', 'reqRobFlag', 'reqUopIdx',
                   'reqLqVal', 'reqLqFlag', 'reqNlsElem', 'reqFuType', 'ldinValid',
                   'ldinLqVal', 'ldinLqFlag', 'ldinNeedRep', 'ldinUpdOk', 'ldinIsvec',
                   'ldinMmio', 'ldinPaddr', 'vcValid', 'vcRobVal', 'vcRobFlag', 'vcUopIdx'):
        ports = [(n, int(w)) for n, w in inputs if n.startswith('io_' + prefix + '_')]
        assert ports and all(w <= 64 for _, w in ports)
        kind = 'CData' if ports[0][1] <= 8 else ('IData' if ports[0][1] <= 32 else 'QData')
        arrays.append(f'  std::array<{kind}*, {len(ports)}> {prefix} = {{' +
                      ', '.join('&d.' + n for n, _ in ports) + '};')
    words = (int(outputs['io_newOut']) + 31) // 32
    compare = ('d.io_newOut != d.io_refOut' if words <= 2 else
               'false ' + ''.join(f'|| d.io_newOut[{i}] != d.io_refOut[{i}] ' for i in range(words)))
    return f'''#include "VQueueContractTest.h"
#include "verilated.h"
#include <array>
#include <cstdlib>
#include <iostream>
#include <random>

int main(int argc, char** argv) {{
  Verilated::commandArgs(argc, argv);
  VQueueContractTest d;
  constexpr int N = {depth};
  int cycles = 0, allocations = 0, vectors = 0, redirects = 0, resets = 0, wraps = 0;
  bool oldPhase = false;
{chr(10).join(arrays)}
  auto idle = [&]() {{
{zero}
    d.io_sqCanAccept = 1;
  }};
  auto check = [&]() {{
    d.eval();
    if (d.io_mismatch || ({compare})) {{
      std::cerr << "miter mismatch at cycle " << cycles << std::endl;
      std::exit(1);
    }}
  }};
  auto tick = [&]() {{
    d.clock = 0; check(); d.clock = 1; check(); d.clock = 0; check();
    ++cycles;
    wraps += d.io_legalLqFlag != oldPhase;
    oldPhase = d.io_legalLqFlag;
  }};
  auto reset = [&]() {{
    idle(); d.io_initDefined = 1; d.reset = 1;
    for (int i = 0; i < 3; ++i) {{ d.clock = 0; d.eval(); d.clock = 1; d.eval(); }}
    d.clock = 0; d.reset = 0; d.io_initDefined = 0; ++resets;
    check();
  }};
  auto request = [&](int lane, int slot, int phase, int length, int rob, bool vector) {{
    *needAlloc[lane] = 1; *reqValid[lane] = 1; *reqRobVal[lane] = rob;
    *reqUopIdx[lane] = 1; *reqLqVal[lane] = slot; *reqLqFlag[lane] = phase;
    *reqNlsElem[lane] = length; *reqFuType[lane] = vector ? {vector_type}ULL : {scalar_type}ULL;
    ++allocations; vectors += vector;
  }};
  auto feedback = [&](int rob) {{ *vcValid[0] = 1; *vcRobVal[0] = rob; *vcUopIdx[0] = 1; }};
  auto wb = [&](int lane, int slot, int address) {{
    *ldinValid[lane] = 1; *ldinUpdOk[lane] = 1; *ldinLqVal[lane] = slot;
    *ldinPaddr[lane] = address; *ldinMmio[lane] = address & 1;
  }};
  auto drain = [&]() {{ idle(); for (int i = 0; i < 7; ++i) tick(); }};
  std::mt19937 random(0x511c);
  reset();
  for (int episode = 0; episode < 600; ++episode) {{
    idle(); check();
    if (!d.io_canAccept) {{ std::cerr << "queue did not drain" << std::endl; return 2; }}
    int start = d.io_legalLqIdx, phase = d.io_legalLqFlag;
    int mode = episode % 5;
    if (mode == 0) {{
      // Three-port dispatch, two-flow vector, same-cycle new-identity feedback.
      request(0, start, phase, 1, 10, false);
      request(1, (start + 1) % N, phase ^ (start + 1 >= N), 2, 11, true);
      request(2, (start + 3) % N, phase ^ (start + 3 >= N), 1, 12, false);
      feedback(11); wb(0, start, random()); tick();
      idle(); feedback(11); wb(0, (start + 3) % N, random()); tick();
    }} else if (mode == 1) {{
      // All-canceled dispatch still reserves before delayed rollback.
      request(0, start, phase, 3, 20, false);
      d.io_redirectValid = 1; d.io_redirectRobVal = 19; ++redirects; tick();
    }} else if (mode == 2) {{
      // Old-state content completion and redirect may affect different fields.
      request(0, start, phase, 2, 30, true); tick();
      idle(); feedback(30); d.io_redirectValid = 1; d.io_redirectRobVal = 29;
      ++redirects; tick();
    }} else {{
      int length = 1 + random() % 3;
      request(0, start, phase, length, 40, false);
      wb(0, start, random()); wb(1, start, random()); tick();
      idle();
      for (int k = 0; k < length; ++k) wb(k, (start + k) % N, random());
      tick();
    }}
    drain();
    if (episode % 97 == 0) reset();
  }}
  if (allocations < 600 || !vectors || !redirects || !wraps || resets < 2) return 3;
  std::cout << "PASS cycles=" << cycles << " allocations=" << allocations
    << " vectors=" << vectors << " redirects=" << redirects << " wraps=" << wraps
    << " resets=" << resets << std::endl;
  d.final();
}}
'''


def async_driver(top_body):
    ports = re.findall(r'^    output (\w+) : ', top_body, re.M)
    metadata, payload = [], []
    for name in ports:
        if not name.startswith('io_currentState_'):
            continue
        comparison = f'd.{name} == d.{name.replace("currentState", "referenceState")}'
        if re.search(r'_(allocated|isvec)_\d+$', name):
            metadata.append(comparison)
        elif re.match(r'io_currentState_(robIdx_0_|(uopIdx|committed|debug_mmio|debug_paddr)_0$)', name):
            payload.append(comparison)
    assert metadata and payload
    return '''#include "VQueueContractTest.h"
#include "verilated.h"
#include <cstdlib>
#include <iostream>
int main(int argc, char** argv) {
  Verilated::commandArgs(argc, argv);
  VQueueContractTest d;
  bool defined = false;
  auto check = [&]() {
    d.eval();
    if (!(METADATA) || (defined && !(PAYLOAD))) {
      std::cerr << "miter mismatch during asynchronous reset" << std::endl;
      std::exit(1);
    }
  };
  auto tick = [&]() { d.clock = 0; check(); d.clock = 1; check(); d.clock = 0; check(); };
  auto offer = [&](int rob, int uop) {
    d.io_allocate = 1; d.io_rob = rob; d.io_uop = uop;
    tick(); defined = true; d.io_allocate = 0; check();
  };
  d.clock = 0; d.reset = 0; d.io_allocate = 0; d.io_rob = 0; d.io_uop = 0;
  d.io_queueReset = 1; tick(); tick(); d.io_queueReset = 0; check();
  offer(42, 3);
  d.io_queueReset = 1; check(); d.io_queueReset = 0; check(); tick();
  offer(51, 9);
  d.io_queueReset = 1; check(); offer(85, 45);
  d.io_queueReset = 0; check(); offer(91, 7);
  std::cout << "PASS asynchronous pulse, held reset and unreset payload writes" << std::endl;
  d.final();
}
'''.replace('METADATA', ' && '.join(metadata)).replace('PAYLOAD', ' && '.join(payload))


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('low_firrtl', type=Path)
    p.add_argument('annotations', type=Path, help='queue-contract JSON or annotated CHIRRTL')
    p.add_argument('rtl', type=Path)
    p.add_argument('output', type=Path)
    p.add_argument('--verilator', default='verilator')
    p.add_argument('--scalar-type', type=int, required=True)
    p.add_argument('--vector-type', type=int, required=True)
    p.add_argument('--async-reset', action='store_true')
    args = p.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    low = args.low_firrtl.read_text()
    annotation_text = args.annotations.read_text()
    anno = (json.JSONDecoder().raw_decode(annotation_text.split(':%[', 1)[1])[0]
            if ':%[' in annotation_text else json.loads(annotation_text))
    contract = extract(low, anno, 'TestableVirtualLoadQueue')
    (args.output / 'contract.json').write_text(json.dumps(contract, indent=2) + '\n')
    model = emit_sv(contract)
    (args.output / 'queue-model.sv').write_text(model)
    rtl = args.rtl.read_text()
    original = re.search(r'module TestableVirtualLoadQueue\(.*?\n\);', rtl, re.S)[0]
    inputs = set(re.findall(r'^\s*input\s+(?:\[[^\]]+\]\s+)?(\w+)', original, re.M))
    used = set()
    for node in contract['nodes'].values():
        used |= refs(node['expr'])
    for expr in contract['observations'].values():
        used |= refs(expr)
    for state in contract['states'].values():
        used |= refs(state['reset']) | refs(state['reset_value']) | refs(state['next']) | {state['clock']}
    for a in contract['assertions']:
        used |= refs(a['enable']) | refs(a['predicate']) | {a['clock']}
    missing = {n for n in used if n in contract['ports'] and
               contract['ports'][n]['direction'] == 'input'} - inputs
    if missing:
        raise ValueError('RTL pruned model inputs; preserve the boundary with dontTouch: ' + ', '.join(sorted(missing)))
    replaced, count = re.subn(r'module TestableVirtualLoadQueue\(.*?\nendmodule',
        lambda _: model.strip(), rtl, count=1, flags=re.S)
    assert count == 1, 'expected exactly one VLQ module'
    top_name = 'VLoadQueueResetHarness' if args.async_reset else 'VLoadQueueMiterHarness'
    top = next(s for s in re.split(r'^  module ', low, flags=re.M)[1:]
               if s.startswith(top_name + ' :'))
    cpp = (async_driver(top) if args.async_reset else
           driver(top, contract['queue']['entries'], args.scalar_type, args.vector_type))
    (args.output / 'driver.cpp').write_text(cpp)
    records = {}
    broken = json.loads(json.dumps(contract))
    fault = 'contents_0_isvec' if args.async_reset else 'contents_0_committed'
    broken['states'][fault]['next'] = dict(op='literal', width=1, signed=False, value=0)
    negative = replaced.replace(model.strip(), emit_sv(broken).strip(), 1)
    for label, source in [('rtl', rtl), ('model', replaced), ('negative', negative)]:
        sv = args.output / (label + '.sv')
        sv.write_text(source)
        cmd = [args.verilator, '--cc', '--exe', '--build', '-j', '2', '-Wno-fatal',
            '-Wno-PINMISSING', '--assert', '--top-module', top_name,
            '--prefix', 'VQueueContractTest', '--Mdir', str(args.output / (label + '-obj')),
            '-CFLAGS', '-std=c++17', str(sv), str(args.output / 'driver.cpp')]
        start = time.monotonic()
        with (args.output / (label + '-build.log')).open('w') as log:
            subprocess.run(cmd, check=True, stdout=log, stderr=subprocess.STDOUT)
        records[label] = dict(command=cmd, build_seconds=time.monotonic() - start)
        start = time.monotonic()
        result = subprocess.run([str(args.output / (label + '-obj/VQueueContractTest'))],
                                text=True, capture_output=True)
        records[label].update(run_seconds=time.monotonic() - start, result=result.stdout,
                              stderr=result.stderr, exit_code=result.returncode)
        (args.output / (label + '-run.log')).write_text(result.stdout + result.stderr)
        if label == 'negative':
            assert result.returncode != 0 and 'miter mismatch' in result.stderr, result
        else:
            result.check_returncode()
    (args.output / 'result.json').write_text(json.dumps(records, indent=2) + '\n')
    print(json.dumps(records, indent=2))


if __name__ == '__main__':
    main()
