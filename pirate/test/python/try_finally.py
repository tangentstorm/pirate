try:
    print '1',
    raise hell
    print '2',
finally:
    print '3',

try:
    print '4',
finally:
    print '5',
