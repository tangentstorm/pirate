a, b, c = (lambda: (1,2,3))()
print a, b, c
a = (lambda: (4,5,6))()
print a
