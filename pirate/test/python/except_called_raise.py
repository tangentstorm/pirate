def raise_exception():
   raise hell
try:
   raise_exception()
   print 'fumbled.'
except:
   print 'caught it!'
