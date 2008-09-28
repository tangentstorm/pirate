emacs 
"""
Functional primitives for python.
"""


def x_assert(test, msg=None): assert test, msg
def x_assign(dict, name, value): dict[name]=value

def x_block(head, *tail):
    res = apply(head)
    return x_block(*tail) if tail else res


def x_class(scope, className, bases, classDict):
    x_assign(scope, className, type(className, bases, classDict))

def x_discard(x): None

def x_else(): True

def x_for(series, block, x_for_else):
    for var in series:
        try:
            block(var)
        except XContinue: continue
        except XBreak : break
    else:
        apply(x_for_else)

def x_if((test, then), *elifs):
    return apply(then) if apply(test) else x_if(*elifs) if elifs else None

def x_print(*args):
    print " ".join([str(a) for a in args])


def x_raise(e): raise e

class XContinue(Exception): pass
def x_continue(): raise XContinue()

class XBreak(Exception): pass
def x_break(): raise XBreak()


# @TODO: x_func for return from middle of series
# class XReturn(Exception): pass
# def x_return(value): raise XReturn(value)

def x_try(try_block, catch_exprs, final_expr):
    for item in try_block:
        try:
            apply(item)
        except Exception, raised:
            for (ex_class, ex_handler) in catch_exprs:
                if isinstance(raised, ex_class):
                    apply(ex_handler, raised)
                    break
            else:
                apply(final_expr)
                raise
    apply(final_expr)


# @TODO: while : break, continue, else, return
def x_while(cond, block, else_block=None):
    assert else_block is None , "@TODO"
    x_if((cond, block))
    x_if((cond, lambda : x_while(cond, block)))


class XYield(Exception): pass
def x_yield(value): raise XYield(value)

class x_gen:
    """
    Defines a generator block.
    @TODO: I don't think this will work inside loops
    """
    def __init__(self, head, *tail):
        self.head = head            
        self.tail = list(tail)

    def _run(self):
        try:
            apply(self.head)
        except XYield:
            raise
        finally:
            if self.tail:
                self.head = self.tail.pop(0)
            else:
                self.head = lambda : x_raise(StopIteration)

    def __iter__(self):
        return self

    def next(self):
        try:
           self._run()
        except XYield, xy:
            return xy.message

