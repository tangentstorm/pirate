"""
weightless threads benchmark for pirate generators.

$Id$

inspired by an article by david mertz:
  http://ibm.com/developerworks/linux/library/l-pythrd.html

"""

from __future__ import generators

RUNS = 1500

# Looks like there's a nasty slowdown going
# on with generators as RUNS increases :/
#
# timings taken Jan 10 2004 by michal:
#
#  RUNS     end          python         pirate
# -------------------------------------------
#   500    24950       0.007462       0.543761
#  1000    49950       0.014791       2.888405
#  1500    74950       0.021666       3.204709 
#  5000   249950       0.074457      67.666664
# 10000   499950       0.163052     559.732823
#
# at RUNS=100000, parrot eventually segfaulted


## python compatibility stuff ###############

def time():
    PARROT_INLINE(
        ".local object t",
        "t = new PerlNum",
        "time N0",
        "t = N0",
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

while ticks < RUNS: 
    ticks = ticks + 1
    for gen in threads:
        name, count = gen.next()
t2 = time()
print "ended with:", name, count
print "total time:", t2 - t1

