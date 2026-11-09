#!/usr/bin/env python3
# SPDX-License-Identifier: MulanPSL-2.0
"""Check rejection boundaries and simulate signed widths/old-state scheduling."""
import copy
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from queue_contract import extract, emit_sv


ANNOTATIONS = [dict(
    **{'class': 'utils.QueueModel.QueueContractAnnotation'}, target='~Probe|Probe',
    entries=1, numEnq=1, numDeq=1, stateRoots=['contents', 'enqPtr', 'deqPtr'])]
FIRRTL = '''circuit Probe :
  module Probe :
    input clock : Clock
    input reset : UInt<1>
    input enable : UInt<1>
    input a : SInt<4>
    input b : SInt<2>
    output sum : SInt<5>
    output mask : UInt<4>
    output complement : UInt<4>
    output less : UInt<1>
    output selected : SInt<5>
    output wrapped : UInt<3>
    output snapshot : SInt<4>
    output previous : SInt<4>
    reg contents : SInt<4>, clock with :
      reset => (reset, SInt<4>(0))
    reg enqPtr : UInt<1>, clock with :
      reset => (reset, UInt<1>(0))
    reg deqPtr : UInt<1>, clock with :
      reset => (reset, UInt<1>(0))
    reg owner_previous : SInt<4>, clock with :
      reset => (reset, SInt<4>(0))
    node added = add(a, b)
    node masked = and(a, b)
    node negated = not(a)
    node compare = lt(a, b)
    node pick = mux(enable, pad(a, 5), pad(b, 5))
    node trunc = bits(asUInt(added), 2, 0)
    sum <= added
    mask <= masked
    complement <= negated
    less <= compare
    selected <= pick
    wrapped <= trunc
    snapshot <= contents
    previous <= owner_previous
    contents <= mux(enable, a, contents)
    enqPtr <= not(enqPtr)
    deqPtr <= enqPtr
    owner_previous <= contents
'''
TESTBENCH = '''module Test;
  logic clock = 0, reset = 1, enable = 0;
  logic signed [3:0] a = 0;
  logic signed [1:0] b = 0;
  wire signed [4:0] sum, selected;
  wire [3:0] mask, complement;
  wire less;
  wire [2:0] wrapped;
  wire signed [3:0] snapshot, previous;
  Probe dut(.*);
  integer old_value = 0;
  initial begin
    #1; clock = 1; #1; clock = 0; reset = 0;
    for (integer i = -8; i < 8; i++) begin
      for (integer j = -2; j < 2; j++) begin
        a = 4'(i); b = 2'(j); enable = 0; #1;
        if ($signed(selected) != j) $fatal(1, "signed mux extension");
        enable = 1; #1;
        if ($signed(sum) != i + j || mask != ((i & j) & 15) ||
            complement != ((~i) & 15) || less != (i < j) ||
            $signed(selected) != i || wrapped != ((i + j) & 7))
          $fatal(1, "width/sign/truncation at %d %d", i, j);
        clock = 1; #1;
        if ($signed(snapshot) != i || $signed(previous) != old_value)
          $fatal(1, "simultaneous old-state commit");
        old_value = i; clock = 0; #1;
      end
    end
    reset = 1; clock = 1; #1;
    if (snapshot != 0 || previous != 0) $fatal(1, "reset priority");
    $display("PASS 64 signed input pairs and old-state commits");
    $finish;
  end
endmodule
'''


class QueueContractTest(unittest.TestCase):
    def test_rejects_missing_state_root(self):
        annotations = copy.deepcopy(ANNOTATIONS)
        annotations[0]['stateRoots'].append('missing')
        with self.assertRaisesRegex(ValueError, 'unresolved queue state root'):
            extract(FIRRTL, annotations, 'Probe')

    def test_rejects_unknown_effect_or_primitive(self):
        for text in (FIRRTL + '    inst hidden of Other\n',
                     FIRRTL.replace('add(a, b)', 'div(a, b)')):
            with self.assertRaisesRegex(ValueError, 'unsupported'):
                extract(text, ANNOTATIONS, 'Probe')

    def test_rejects_missing_or_unlowered_connect(self):
        for text in (FIRRTL.replace('    contents <= mux(enable, a, contents)\n', ''),
                     FIRRTL + '    contents <= a\n'):
            with self.assertRaises(ValueError):
                extract(text, ANNOTATIONS, 'Probe')

    def test_rejects_unresolved_or_abstract_types(self):
        for text in (FIRRTL.replace('add(a, b)', 'add(a, missing)'),
                     FIRRTL.replace('input reset : UInt<1>', 'input reset : Reset')):
            with self.assertRaises(ValueError):
                extract(text, ANNOTATIONS, 'Probe')

    def test_signed_widths_and_state_commit(self):
        contract = extract(FIRRTL, ANNOTATIONS, 'Probe')
        self.assertEqual(contract['states']['owner_previous']['owner'], 'owner')
        with tempfile.TemporaryDirectory(prefix='queue-contract-') as tmp:
            root = Path(tmp)
            source = root / 'test.sv'
            source.write_text(emit_sv(contract) + TESTBENCH)
            cmd = [os.environ.get('VERILATOR', 'verilator'), '--binary', '--timing',
                   '--assert', '-Wno-fatal', '--top-module', 'Test', '--Mdir', str(root / 'obj'), str(source)]
            subprocess.run(cmd, check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            result = subprocess.run([str(root / 'obj/VTest')], check=True, text=True,
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            self.assertIn('PASS 64 signed input pairs', result.stdout)


if __name__ == '__main__':
    unittest.main()
