from __future__ import generators #@TODO: 2.3
def make_repeater(s):
    def repeat():
	while 1:
	    yield s
    return repeat()
a = make_repeater('a')
b = make_repeater('b')
for x in [1,2,3,4]:
    print a.next(),
    print b.next(),
