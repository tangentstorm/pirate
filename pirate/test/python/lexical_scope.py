x = 1
def make_adder(base):
    def adder(x):
	return base+x
    return adder
h = make_adder(10)
print h(5), x
