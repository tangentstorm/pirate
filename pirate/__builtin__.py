
# stolen from PyPy's builtin module:
# http://codespeak.net/viewcvs.cgi/pypy/trunk/src/pypy/module/


def abs(x): return x.__abs__()
def cmp(x,y): return x.__cmp__(y)
def float(x): return x.__float__()
def hex(x): return x.__hex__()
def int(x): return x.__int__()
def oct(x): return x.__oct__()

#def range(x, y=None, step=1):
def range(x, y, step):    
    """
    returns a list of integers in arithmetic position from start (defaults
    to zero) to stop - 1 by step (defaults to 1).  Use a negative step to
    get a list in decending order.
    """
    # @TODO: correct the 'is' op
    #if y is None:
    #    start = 0
    #    stop = x
    #else:
    start = x
    stop = y

    if step == 0:
        #@TODO: raise ValueError, 'range() arg 3 must not be zero'
        raise 'range() arg 3 must not be zero'
        
    elif step > 0:
        if stop <= start: # no work for us
            return []
        howmany = (stop - start + step - 1)/step
        
    else:  # step must be < 0, or we would have raised ValueError
        if stop >= start: # no work for us
            return []
        howmany = (start - stop - step - 1)/-step

    PARROT_INLINE('find_lex $P0, "howmany"')
    PARROT_INLINE('$I0 = $P0')
    PARROT_INLINE('.local PyList arr')
    PARROT_INLINE('arr = new PyList ') 
    PARROT_INLINE('set arr, $I0') 
    #@TODO: arr = [None] * howmany  # this is to avoid using append.

    i = start
    n = 0
    PARROT_INLINE('.local int n')
    while n < howmany:
        PARROT_INLINE('find_lex $P0, "i"')
        PARROT_INLINE('find_lex $P1, "n"')
        PARROT_INLINE('n = $P1')
        PARROT_INLINE('arr[n] = $P0')
        i += step
        n += 1

    PARROT_INLINE('.pcc_begin_return')
    PARROT_INLINE('.return arr')
    PARROT_INLINE('.pcc_end_return')
