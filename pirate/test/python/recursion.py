def f(x):
    print x,
    if x==0:
	return 0
    else:
	return f(x-1)
f(1)
