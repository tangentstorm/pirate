class Thing:
    z = 0
t1 = Thing()
t1.x = 1
t2 = Thing()
t2.x = 2
print t1.x, t2.x,
print t1.z, t2.z,
print t1.__class__.__name__
