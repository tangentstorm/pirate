for num in [1, 2, 3, 4, 5]:
    print num * num,
print
for x in []: print "FAILED"
a = 0
for x in 1,2,3:
    a += 1
    if a > 3: print "FAILED"; break
    continue # loop should iterate after each continue
