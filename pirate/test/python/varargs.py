args={'a':1,'b':2,'c':3}
def f(a,b,c): return (a,b,c)
def g(b,c,a): return (a,b,c)
for j in [f,g]: print j(1,2,3)
for j in [f,g]: print j(a=1,b=2,c=3)
for j in [f,g]: print j(*[1,2,3])
for j in [f,g]: print j(**args) 
