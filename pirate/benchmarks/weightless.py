"""
inspired by weightless threads article by david mertz
http://www-106.ibm.com/developerworks/linux/library/l-pythrd.html
"""

from __future__ import generators


## python compatibility stuff ###############
def PARROT_INLINE(*args): pass # ignored by pirate
def time():
    PARROT_INLINE(
        ".local object t",
        "t = new PerlNum",
        "time I0",
        "t = I0",
        ".pcc_begin_return",
        ".return t",
        ".pcc_end_return",
        )
from time import time # ignored by pirate
#############################################

def make_counter(name, step):
    def counter():
        count = 0
        while 1:
            yield [name, count]
            count = count + step
    return counter()
    

threads = [
    make_counter("I",  1),
    make_counter("V",  5),
    make_counter("X", 10),
    make_counter("L", 50),
]


t1 = time()
ticks = 0
while ticks < 100000:
    ticks = ticks + 1
    for gen in threads:
        name, count = gen.next()
t2 = time()
print "ended with:", name, count
print "total time:", t2 - t1
