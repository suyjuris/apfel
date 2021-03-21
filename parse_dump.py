#!/usr/bin/python3

import sys
import zlib
import json
import subprocess
import base64

if sys.argv[1] == 'yafc_dump2':
    data = json.loads(zlib.decompress(base64.b64decode(subprocess.run(['xclip', '-o'], stdout=subprocess.PIPE).stdout), -15))
    print(json.dumps(data, sort_keys=True, indent=4))
elif sys.argv[1] == 'yafc_dump':
    data = zlib.decompress(base64.b64decode(subprocess.run(['xclip', '-o'], stdout=subprocess.PIPE).stdout), -15)
    print(data.decode('utf-8'))
else:
    assert False
    


#data = json.loads(zlib.decompress(base64.b64decode(subprocess.run(['xclip', '-o'], stdout=subprocess.PIPE).stdout[1:])).decode('utf-8'))
#print(json.dumps(data, sort_keys=True, indent=4))

#f = open(sys.argv[1], 'rb')
#print((b'0' + base64.b64encode(zlib.compress(f.read()))).decode('utf-8'))
#f.close()
