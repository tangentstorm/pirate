for a in [0,1,2,3,4]:
    if a % 2: continue
    print a,
else: print "OK"
for a in [1]: break
else: print "FAILED"
#@TODO: Would be nice to check break-from-else too, like this:
#for a in [1]: pass
#else: break # should be a SyntaxError
