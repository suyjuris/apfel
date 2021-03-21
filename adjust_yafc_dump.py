#!/usr/bin/python3

import sys
import zlib
import json
import subprocess
import base64
import os

f = open(sys.argv[1], 'rb')
qq = zlib.decompress(base64.b64decode(f.read()), -15).decode('utf-8')
f.close()

lines = qq.split('\n')
data = json.loads(lines[5])

f = open('partition.out', 'r')
partition = [i.strip().split(' ') for i in f]
f.close()

links = []
recipes = {}

def do_data(data):
    for rec in data['recipes']:
        rec2 = rec.copy()
        rec2['subgroup'] = None
        recipes[rec2['recipe'][len('Recipe.'):]] = rec2

        if rec['subgroup']:
            do_data(rec['subgroup'])
    for link in data['links']:
        links.append(link)

do_data(data['content'])
                
rec_list = []
for part in partition:
    first = recipes[part[0]]
    subgroup = {
        'expanded': False,
        'links': [],
        'recipes': [],
        'modules': None
    }
    first['subgroup'] = subgroup

    for rest in part[1:]:
        subgroup['recipes'].append(recipes[rest])
    rec_list.append(first)

data2 = data.copy()
data2['content']['recipes'] = rec_list
data2['content']['links'] = links
data2['guid'] = os.urandom(16).hex()
data2['name'] += '2'

#json.dump(data2, sys.stdout, indent=2)
#print()

def compress(a):
    obj = zlib.compressobj(wbits = -15)
    result = obj.compress(a)
    result += obj.flush()
    return result

w = '\n'.join(lines[:5]).encode('ascii') + b'\n' + json.dumps(data2, separators=(',', ':')).encode('utf-8')
q = base64.b64encode(compress(w))
print(q.decode('utf-8'), end='')
