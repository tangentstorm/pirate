from __future__ import generators #@TODO: 2.3
def count():
    x = 0
    while 1:
	yield x
	x = x + 1
g = count()
for i in [1,2,3]:
    print g.next(),
