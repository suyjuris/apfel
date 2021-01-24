#!/usr/bin/python3


import sys
import json
import zlib
import base64
from collections import defaultdict
import subprocess
from math import floor, ceil
import textwrap

blueprint = subprocess.run(['xclip', '-o', '-selection', 'clipboard'], stdout=subprocess.PIPE).stdout.decode('ascii')
if blueprint[0] != '0':
    print('Error: Clipboard is not a valid blueprint!', file=sys.stdout)
    sys.exit(5)
data = json.loads(zlib.decompress(base64.b64decode(blueprint[1:])).decode('utf-8'))

class Field:
    EMPTY=0
    BELT=1
    SPLIT=2
    UNDER=3
    
    def __init__(self):
        self.inp = None
        self.out = None
        self.typ = Field.EMPTY

def move(x,y,d): return [(x,y+1),(x+1,y),(x,y-1),(x-1,y)][d]
def moveb(x,y,d): return [(x,y-1),(x-1,y),(x,y+1),(x+1,y)][d]

class Instance:
    def __init__(self, nx, ny, scale_fac, do_blocking, yoff_output, yoff_input):
        self.nx = nx
        self.ny = ny
        self.scale_fac = scale_fac
        self.do_blocking = do_blocking
        self.yoff_output = tuple(yoff_output)
        self.yoff_input = tuple(yoff_input)
    def __eq__(a, b):
        return (a.nx == b.nx and a.ny == b.ny and a.scale_fac == b.scale_fac and
                a.do_blocking == b.do_blocking and a.yoff_output == b.yoff_output and
                a.yoff_input == b.yoff_input)
    def __hash__(self):
        x = 0
        x = hash((x, self.nx))
        x = hash((x, self.ny))
        x = hash((x, self.scale_fac))
        x = hash((x, self.do_blocking))
        x = hash((x, self.yoff_output))
        x = hash((x, self.yoff_input))
        return x
    def __str__(self):
        return textwrap.dedent(f'''\
        {{
            nx = {self.nx}, ny = {self.ny}, scale_fac = {self.scale_fac}, do_blocking = {self.do_blocking},
            yoff_output = {list(self.yoff_output)}, yoff_input = {list(self.yoff_input)}
        }}''')

def print_balancer_single(blueprint, instances, instance_names):
    ents = blueprint['entities']
    dirs = {2:1, 6:3, 0:2, 4:0}
    fields = defaultdict(Field)

    for ent in ents:
        x = ent['position']['x']
        y = ent['position']['y']
        d = dirs[ent.get('direction', 0)]

        if ent['name'].endswith('-splitter'):
            x1, y1 = int(floor(x)), int(floor(y))
            x2, y2 = int(ceil (x)), int(ceil (y))
            fields[x1,y1].typ = Field.SPLIT
            fields[x1,y1].out = d
            fields[x1,y1].other = x2, y2
            fields[x2,y2].typ = Field.SPLIT
            fields[x2,y2].out = d
            fields[x2,y2].other = x1, y1
        elif ent['name'].endswith('-underground-belt'):
            fields[x,y].typ = Field.UNDER
            if   ent['type'] == 'output': fields[x,y].out = d
            elif ent['type'] == 'input':  fields[x,y].inp = d
            else: assert False
        elif ent['name'].endswith('-transport-belt'):
            fields[x,y].typ = Field.BELT
            fields[x,y].out = d

    d_total = None
    for (x,y),f in fields.items():
        if f.out is None: continue
        xx, yy = move(x, y, f.out)
        if (fields.get((xx, yy), Field()).typ == Field.EMPTY
            and (f.typ != Field.SPLIT or fields.get(move(f.other[0], f.other[1], f.out), Field()).typ == Field.EMPTY)):
            assert d_total is None or d_total == f.out
            d_total = f.out
    assert d_total is not None
    
    turn = (1 - d_total + 4) % 4
    if turn:
        fields2 = defaultdict(Field)
        for (x,y),f in fields.items():
            xx, yy = [None,(y,-x),(-x,-y),(-y,x)][turn]
            if f.out is not None: f.out = (f.out + turn) % 4
            if f.inp is not None: f.inp = (f.inp + turn) % 4
            if f.typ == Field.SPLIT:
                x1, y1 = f.other
                f.other = [None,(y1,-x1),(-x1,-y1),(-y1,x1)][turn]
            fields2[xx, yy] = f
        fields = fields2
            
    min_x = float('inf')
    max_x = float('-inf')
    for (x,y),f in fields.items():
        if f.typ == Field.BELT and f.out == 1: continue
        if f.typ == Field.EMPTY: continue # Should not happen
        min_x = min(min_x, x)
        max_x = max(max_x, x)

    fields = {(x,y):f for (x,y),f in fields.items() if min_x <= x <= max_x}

    min_y = float('inf')
    max_y = float('-inf')
    for (x,y),f in fields.items():
        if f.typ == Field.EMPTY: continue # Should not happen
        min_y = min(min_y, y)
        max_y = max(max_y, y)

    notinputs = []
    outputs = []
    for (x,y),f in fields.items():
        if f.out is None: continue
        if f.typ == Field.UNDER:
            notinputs.append((x, y))
        
        xx, yy = move(x, y, f.out)
        notinputs.append((xx, yy))
        f2 = fields.get((xx,yy), Field())
        if f2.typ == Field.SPLIT:
            notinputs.append(f2.other)

        if xx > max_x: outputs.append(y - min_y)
        
    inputs = [y-min_y for (x,y),f in fields.items() if f.out is not None and (x,y) not in notinputs]

    inst = Instance(max_x+1-min_x, max_y+1-min_y, 1, 0, inputs, outputs)
    if inst not in instances:
        name = f'{len(inputs)}to{len(outputs)}'
        if name in instance_names:
            i = 1
            while name + '_' + str(i) in instance_names: i += 1
            name += '_' + str(i)
        instance_names.append(name)
        sols = []
        instances[inst] = name, sols

        print(f'instance {name} {inst}')
    else:
        name, sols = instances[inst]

    solname = 'sol' + str(len(sols) + 1)
    sols.append(solname)
        
    print(f'solution {name} {solname} {{')
        
    s = 'v>^<'
    for y in range(min_y, max_y+1):
        #print(' #> ' if y in inputs else '    ', end='')
        print('    ', end='')
        for x in range(min_x, max_x+1):
            f = fields.get((x,y), Field())
            if   f.typ == Field.EMPTY: print('..', end='')
            elif f.typ == Field.BELT:  print('.' + s[f.out], end='')
            elif f.typ == Field.SPLIT: print('S' + s[f.out], end='')
            elif f.typ == Field.UNDER:
                if   f.inp is not None: print(s[f.inp] + 'u', end='')
                elif f.out is not None: print('u' + s[f.out], end='')
                else: assert False
            else:
                assert False
        #print(' >#' if y in outputs else '')
        print()
    print('}')

def print_balancer_book(book):
    instances = {}
    instances_names = []
    for obj in book['blueprint_book']['blueprints']:
        bp = obj['blueprint']
        #print('#', bp['label'])
        print_balancer_single(bp, instances, instances_names)
        print()

print_balancer_book(data)
