#!/usr/bin/env python
"""
just a dumb little script that can (sometimes)
give more useful output that parrot -t

run it with:

   traceparrot.py filename.imc

"""

import sys, os

try:
    filename = sys.argv[1]
except:
    print "please give a filename"
    sys.exit()

if not os.path.exists(filename):
    print "file not found"
    sys.exit()

def esc(s):
    return s.replace("\\","\\\\").replace('"', '\\"')

## first add the line numbers
lines = open(filename).read().split("\n")
newcode = []
for i in range(len(lines)):
    line = lines[i]
    if line:        
        if line[0]==" " and not line.strip().startswith("."):
            newcode.append('print "[line %04i]: %s\\n"' % (i, esc(line)))        
        newcode.append(line)

## now run it
i,o = os.popen4("parrot -")
print >> i, "\n".join(newcode)
i.close()    
print o.read()
