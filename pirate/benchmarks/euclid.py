# Python implementation of Euclid's algorithm

m = 96
n = 64
print m,n

r = m % n
while r != 0:
    m = n
    n = r
    r = m % n
    
print n
    
