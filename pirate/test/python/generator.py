from __future__ import generators #@TODO: 2.3
def count():
    yield 0
    yield 1
gen = count()
try:
    print gen.next(),
    print gen.next(),
    print gen.next(),
except:
    print 'done'
