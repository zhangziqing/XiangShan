#!/usr/bin/env python3
# SPDX-License-Identifier: MulanPSL-2.0
"""Extract an annotated queue's closed transition contract and emit simulation SV.

Input is Low FIRRTL plus Chisel annotations (JSON or annotated CHIRRTL). This
supports only a checked expression subset: unsupported constructs fail closed.
The exported boundary is the complete owning module, including required owner
registers. Queue and owner state remain separately identified in the contract.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re


def split_args(text):
    depth = 0
    start = 0
    result = []
    for i, char in enumerate(text):
        depth += (char == '(') - (char == ')')
        if char == ',' and depth == 0:
            result.append(text[start:i].strip())
            start = i + 1
    return result + [text[start:].strip()]


def parse_expr(text):
    literal = re.fullmatch(r'(UInt|SInt)<(\d+)>\(("[hob][0-9a-fA-F]+"|-?\d+)\)', text)
    if literal:
        kind, width, value = literal.groups()
        value = (int(value[2:-1], {'h': 16, 'o': 8, 'b': 2}[value[1]])
                 if value.startswith('"') else int(value))
        return dict(op='literal', width=int(width), signed=kind == 'SInt', value=value)
    if re.fullmatch(r'\w+', text):
        return dict(op='ref', name=text)
    call = re.fullmatch(r'(\w+)\((.*)\)', text)
    if not call:
        raise ValueError('unsupported expression: ' + text)
    op, args = call.groups()
    return dict(op=op, args=[int(x) if re.fullmatch(r'\d+', x) else parse_expr(x)
                              for x in split_args(args)])


def refs(expr):
    if expr['op'] == 'ref':
        return {expr['name']}
    return set().union(*(refs(x) for x in expr.get('args', []) if isinstance(x, dict)))


def type_info(text):
    if text in ('Clock', 'AsyncReset'):
        return dict(width=1, signed=False, kind=text)
    match = re.fullmatch(r'(UInt|SInt)<(\d+)>', text)
    if not match or int(match[2]) <= 0:
        raise ValueError('unsupported type: ' + text)
    return dict(width=int(match[2]), signed=match[1] == 'SInt', kind=match[1])


def infer(expr, types):
    op = expr['op']
    if op == 'ref':
        return types[expr['name']]
    if op == 'literal':
        return dict(width=expr['width'], signed=expr['signed'])
    args = expr['args']
    signatures = {
        **dict.fromkeys(('add', 'sub', 'mul', 'and', 'or', 'xor', 'eq', 'neq',
                         'lt', 'leq', 'gt', 'geq', 'cat'), (2, 0)),
        **dict.fromkeys(('asUInt', 'asSInt', 'not', 'orr', 'andr', 'xorr'), (1, 0)),
        **dict.fromkeys(('pad', 'head', 'tail', 'shl', 'shr'), (1, 1)),
        'bits': (1, 2), 'mux': (3, 0), 'validif': (2, 0)}
    if op not in signatures:
        raise ValueError('unsupported primitive: ' + op)
    expressions, constants = signatures[op]
    if (len(args) != expressions + constants or
        not all(isinstance(a, dict) for a in args[:expressions]) or
        not all(isinstance(a, int) and a >= 0 for a in args[expressions:])):
        raise ValueError('invalid primitive arguments: ' + op)
    ts = [infer(a, types) for a in args if isinstance(a, dict)]
    ws = [t['width'] for t in ts]
    signed = ts[0]['signed']
    if op in ('mux', 'validif'):
        if ws[0] != 1 or signed or (op == 'mux' and ts[1]['signed'] != ts[2]['signed']):
            raise ValueError('invalid selection types: ' + op)
    elif expressions == 2 and ts[0]['signed'] != ts[1]['signed']:
        raise ValueError('mixed signedness: ' + op)
    if op == 'bits' and not (0 <= args[2] <= args[1] < ws[0]):
        raise ValueError('invalid extraction bounds')
    if op == 'head' and not (0 < args[1] <= ws[0]):
        raise ValueError('invalid head width')
    if op in ('mux', 'validif'):
        signed = ts[1]['signed']
        width = max(ws[1:])
    elif op in ('eq', 'neq', 'lt', 'leq', 'gt', 'geq', 'orr', 'andr', 'xorr'):
        width, signed = 1, False
    elif op in ('add', 'sub'):
        width = max(ws) + 1
    elif op == 'mul':
        width = sum(ws)
    elif op in ('and', 'or', 'xor'):
        width, signed = max(ws), False
    elif op == 'cat':
        width, signed = sum(ws), False
    elif op == 'bits':
        width, signed = args[1] - args[2] + 1, False
    elif op == 'head':
        width, signed = args[1], False
    elif op == 'tail':
        width, signed = ws[0] - args[1], False
    elif op == 'pad':
        width = max(ws[0], args[1])
    elif op in ('asUInt', 'asSInt', 'not'):
        width, signed = ws[0], op == 'asSInt'
    elif op in ('shr', 'shl'):
        width = max(1, ws[0] - args[1]) if op == 'shr' else ws[0] + args[1]
    else:
        raise ValueError('unsupported primitive: ' + op)
    if width <= 0:
        raise ValueError('zero-width expression')
    return dict(width=width, signed=signed)


def sv_expr(expr, types):
    op = expr['op']
    if op == 'ref':
        return expr['name']
    if op == 'literal':
        sign = 's' if expr['signed'] else ''
        return f"{expr['width']}'{sign}h{expr['value'] % (1 << expr['width']):x}"
    args = expr['args']
    es = [sv_expr(a, types) if isinstance(a, dict) else str(a) for a in args]
    width = infer(expr, types)['width']
    binary = dict(add='+', sub='-', mul='*', and_='&', or_='|', xor='^',
                  eq='==', neq='!=', lt='<', leq='<=', gt='>', geq='>=')
    key = op + '_' if op in ('and', 'or') else op
    if key in binary:
        result = f'({es[0]} {binary[key]} {es[1]})'
    elif op == 'mux':
        result = f'({es[0]} ? {es[1]} : {es[2]})'
    elif op == 'validif':
        result = f"({es[0]} ? {es[1]} : {width}'bx)"
    elif op == 'cat':
        result = '{' + ', '.join(es) + '}'
    elif op in ('bits', 'head', 'tail'):
        shift = args[2] if op == 'bits' else (infer(args[0], types)['width'] - args[1] if op == 'head' else 0)
        result = f'($unsigned({es[0]}) >> {shift})'
    elif op == 'pad':
        result = es[0]
    elif op in ('asUInt', 'asSInt'):
        result = f"${'signed' if op == 'asSInt' else 'unsigned'}({es[0]})"
    elif op in ('not', 'orr', 'andr', 'xorr'):
        result = f"({dict(not_='~', orr='|', andr='&', xorr='^')[op if op != 'not' else 'not_']}{es[0]})"
    elif op in ('shr', 'shl'):
        result = f"({es[0]} {'>>>' if op == 'shr' else '<<'} {es[1]})"
    else:
        raise ValueError('unsupported primitive: ' + op)
    # Explicit sizing prevents an enclosing SV expression changing FIRRTL widths.
    cast = '$signed' if infer(expr, types)['signed'] else '$unsigned'
    return f"{cast}({width}'({result}))"


def extract(firrtl, annotations, module):
    tags = [a for a in annotations if a['class'].endswith('.QueueContractAnnotation')
            and a['target'].split('|')[-1] == module]
    if len(tags) != 1:
        raise ValueError('expected exactly one queue annotation for ' + module)
    tag = tags[0]
    sections = re.split(r'^  module ', firrtl, flags=re.M)[1:]
    body = next(s for s in sections if s.split(' :', 1)[0] == module)
    ports, registers, nodes, connects = {}, {}, {}, {}
    locations = {}
    assertions = []
    lines = iter(body.splitlines()[1:])
    for raw in lines:
        line = raw.split(' @[')[0].strip()
        if not line or line == 'skip':
            continue
        match = re.fullmatch(r'(input|output) (\w+) : (.+)', line)
        if match:
            ports[match[2]] = dict(direction=match[1], **type_info(match[3]))
            locations[match[2]] = raw.partition(' @[')[2].rstrip(']')
            continue
        match = re.fullmatch(r'reg (\w+) : (.+), (\w+) with :', line)
        if match:
            reset_line = next(lines)
            reset = reset_line.strip().split(' @[')[0]
            assert reset.startswith('reset => (') and reset.endswith(')'), reset
            condition, value = split_args(reset[len('reset => ('):-1])
            registers[match[1]] = dict(**type_info(match[2]), clock=match[3],
                reset=parse_expr(condition), reset_value=parse_expr(value))
            locations[match[1]] = reset_line.partition(' @[')[2].rstrip(']')
            continue
        match = re.fullmatch(r'node (\w+) = (.+)', line)
        if match:
            nodes[match[1]] = parse_expr(match[2])
            locations[match[1]] = raw.partition(' @[')[2].rstrip(']')
            continue
        match = re.fullmatch(r'(\w+) <= (.+)', line)
        if match:
            if match[1] in connects:
                raise ValueError('multiple connects require lowering: ' + match[1])
            connects[match[1]] = parse_expr(match[2])
            continue
        if line.startswith('printf('):
            continue
        if line.startswith('assert('):
            clock, predicate, enable, *_ = split_args(line[len('assert('):line.rfind(')')])
            assertions.append(dict(clock=clock, predicate=parse_expr(predicate), enable=parse_expr(enable)))
            continue
        raise ValueError('unsupported statement: ' + line[:180])
    for root in tag['stateRoots']:
        if not any(n == root or n.startswith(root + '_') for n in registers):
            raise ValueError('unresolved queue state root: ' + root)
    if connects.keys() - (ports.keys() | registers.keys()):
        raise ValueError('connection to undeclared state or output')
    expected = registers.keys() | {n for n, p in ports.items() if p['direction'] == 'output'}
    if connects.keys() != expected:
        raise ValueError('missing next state/output or connection to an input')
    types = {**ports, **registers}
    pending = dict(nodes)
    order = []
    while pending:
        ready = [n for n, e in pending.items() if refs(e) <= types.keys()]
        if not ready:
            raise ValueError('unresolved dependencies: ' + ', '.join(list(pending)[:5]))
        for name in ready:
            types[name] = infer(pending.pop(name), types)
            order.append(name)
    for expr in connects.values():
        infer(expr, types)
    for r in registers.values():
        infer(r['reset_value'], types)
        reset_type = infer(r['reset'], types)
        if reset_type['width'] != 1 or reset_type['signed']:
            raise ValueError('reset must have a concrete one-bit type')
        if types[r['clock']].get('kind') != 'Clock':
            raise ValueError('unsupported clock expression')
    for a in assertions:
        if types[a['clock']].get('kind') != 'Clock':
            raise ValueError('unsupported assertion clock')
        for expr in (a['predicate'], a['enable']):
            t = infer(expr, types)
            if t['width'] != 1 or t['signed']:
                raise ValueError('assertion requires one-bit predicates')
    # Include the complete observable state closure, without retaining dead
    # redirect/debug registers just because they share the source RTL module.
    required = {n for n, p in ports.items() if p['direction'] == 'output'}
    required |= {n for n in registers if any(n == r or n.startswith(r + '_') for r in tag['stateRoots'])}
    for a in assertions:
        required |= refs(a['predicate']) | refs(a['enable']) | {a['clock']}
    work = list(required)
    while work:
        name = work.pop()
        expression = nodes.get(name, connects.get(name))
        dependencies = refs(expression) if expression else set()
        if name in registers:
            r = registers[name]
            dependencies |= refs(r['reset']) | refs(r['reset_value']) | {r['clock']}
        for dep in dependencies - required:
            required.add(dep)
            work.append(dep)
    states = {}
    for name, r in registers.items():
        if name in required:
            states[name] = dict(**r, next=connects[name],
                owner='queue' if any(name == x or name.startswith(x + '_') for x in tag['stateRoots']) else 'owner')
    return dict(schema=1, boundary='owning-module', queue=tag, ports=ports, states=states,
        nodes={n: dict(type=types[n], expr=nodes[n]) for n in order if n in required},
        observations={n: connects[n] for n, p in ports.items() if p['direction'] == 'output'},
        assertions=assertions,
        source_locations={n: s for n, s in locations.items() if n in required or n in ports},
        source_sha256=hashlib.sha256(firrtl.encode()).hexdigest())


def emit_sv(contract):
    ports, states, nodes = (contract[k] for k in ('ports', 'states', 'nodes'))
    types = {**ports, **states, **{n: d['type'] for n, d in nodes.items()}}
    def declaration(name, t):
        return f"logic {'signed ' if t['signed'] else ''}[{t['width'] - 1}:0] {name}"
    module = contract['queue']['target'].split('|')[-1]
    result = ['// Generated from the annotated queue transition contract; simulation only.',
              f'module {module} (', ',\n'.join('  ' + p['direction'] + ' ' + declaration(n, p) for n, p in ports.items()), ');']
    for name, state in states.items():
        result.append(declaration(name, state) + ';')
    for name, node in nodes.items():
        result += [declaration(name, node['type']) + ';', f"assign {name} = {sv_expr(node['expr'], types)};"]
    for name, expr in contract['observations'].items():
        result.append(f'assign {name} = {sv_expr(expr, types)};')
    for name, state in states.items():
        reset = sv_expr(state['reset'], types)
        async_reset = infer(state['reset'], types).get('kind') == 'AsyncReset'
        sensitivity = 'posedge ' + state['clock'] + (' or posedge ' + reset if async_reset else '')
        result += [f'always @({sensitivity}) begin',
            f"  if ({reset}) {name} <= {sv_expr(state['reset_value'], types)};",
            f"  else {name} <= {sv_expr(state['next'], types)};", 'end']
    for a in contract['assertions']:
        result += [f"always @(posedge {a['clock']})",
            f"  if ({sv_expr(a['enable'], types)}) assert ({sv_expr(a['predicate'], types)});"]
    return '\n'.join(result + ['endmodule', ''])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('firrtl', type=Path)
    parser.add_argument('annotations', type=Path)
    parser.add_argument('module')
    parser.add_argument('output', type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    annotation_text = args.annotations.read_text()
    annotations = (json.JSONDecoder().raw_decode(annotation_text.split(':%[', 1)[1])[0]
                   if ':%[' in annotation_text else json.loads(annotation_text))
    contract = extract(args.firrtl.read_text(), annotations, args.module)
    (args.output / 'contract.json').write_text(json.dumps(contract, indent=2) + '\n')
    (args.output / 'model.sv').write_text(emit_sv(contract))


if __name__ == '__main__':
    main()
