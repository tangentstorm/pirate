a = 31
b = -13
c = 2

print "__abs__", a.__abs__(), abs(a)
print "__add__", a.__add__(b), a + b
print "__and__", a.__and__(b), a & b
# print "__class__", a.__class__(b)
print "__cmp__", a.__cmp__(b), cmp(a,b)
# print "__coerce__", a.__coerce__(b)
# print "__delattr__", a.__delattr__(b)
print "__div__", a.__div__(b), a / b
# print "__divmod__", a.__divmod__(b), divmod(a,b)
# print "__doc__", a.__doc__(b)
print "__float__", a.__float__(), float(a)
print "__floordiv__", a.__floordiv__(b), a // b
# print "__getattribute__", a.__getattribute__(b)
# print "__getnewargs__", a.__getnewargs__(b)
# print "__hash__", a.__hash__(b)
print "__hex__", a.__hex__(), hex(a)
# print "__init__", a.__init__(b)
print "__int__", a.__int__(), int(a)
print "__invert__", a.__invert__(), ~ a
# print "__long__", a.__long__(), long(a)
print "__lshift__", a.__lshift__(3), a << 3
print "__mod__", a.__mod__(b), a % b
print "__mul__", a.__mul__(b), a * b
print "__neg__", a.__neg__(), - a
# print "__new__", a.__new__(b)
print "__nonzero__", bool(a.__nonzero__())
print "__oct__", a.__oct__(), oct(a)
print "__or__", a.__or__(b), a | b
print "__pos__", a.__pos__(), +a
print "__pow__", a.__pow__(2), a ** 2
print "__radd__", a.__radd__(b), b + a
print "__rand__", a.__rand__(b), b & a
print "__rdiv__", a.__rdiv__(b), b / a
# print "__rdivmod__", a.__rdivmod__(b), divmod(b, a)
# print "__reduce__", a.__reduce__(b)
# print "__reduce_ex__", a.__reduce_ex__(b)
print "__repr__", a.__repr__()
print "__rfloordiv__", a.__rfloordiv__(b), b // a
print "__rlshift__", c.__rlshift__(a), a << 2
print "__rmod__", a.__rmod__(b),  b % a
print "__rmul__", a.__rmul__(b), b * a
print "__ror__", a.__ror__(b), b | a
print "__rpow__", c.__rpow__(a), a ** 2
print "__rrshift__", c.__rrshift__(a), a >> 2
print "__rshift__", a.__rshift__(2), a >> 2
print "__rsub__", a.__rsub__(b), b - a
# print "__rtruediv__", a.__rtruediv__(b)
print "__rxor__", a.__rxor__(b), b ^ a
# print "__setattr__", a.__setattr__(b)
print "__str__", a.__str__(), a
print "__sub__", a.__sub__(b), a - b
print "__truediv__", a.__truediv__(b)
print "__xor__", a.__xor__(b), a ^ b
