"""
ecdysis.scheme module

provides basic data types and functions
inspired by scheme.
"""

from StringIO import StringIO

# * cons

class cons(list):
    """
    the cons cell is the basic data type in
    lisp and scheme. it's just a pair of objects.
    """
    def __init__(self, head, tail):
        self.extend([head,tail])

    def head(self):
        return self[0]

    def tail(self):
        return self[1]

    def split(self):
        """
        return the head and tail at once
        """
        return (self[0], self[1])

    def __iter__(self):
        """
        this allows us to use an ecdysis
        list like a normal python list
        """
        yield self.head()
        if self.tail() is not null:
            for item in self.tail():
                yield item
            
    def to_list(self):
        """
        returns a normal python list
        """
        return [x for x in self]

# to simplify the reader (how?)        
cons.type = cons 
# * head
def head(cell):
    """
    return the first element in a list
    scheme calls this car() for historical reasons
    """
    return cell[0]
# * tail
def tail(cell):
    """
    returns everything after the head
    scheme calls this cdr() for historical reasons
    """
    return cell[1]
# * reverse
def reverse(el):
    """
    reverse an ecdysis list
    """
    # implementation taken from:
    # http://www.cs.sfu.ca/CC/310/pwfong/Lisp/2/tutorial2.html
    def _rev_helper(el, a):
        if el is null:
            return a
        return _rev_helper(tail(el), cons(head(el), a))
    return _rev_helper(el, null)
# * listP
def listP(value):
    """
    is value an ecdysis list?
    """
    return isinstance(value, cons) and isinstance(tail(value), cons)


# * undef, null

class undef: pass
class null(cons):
    """
    null object (in lisp, this is the empty list)
    null marks the end of a list
    """
    def __init__(self):
        cons.__init__(self, undef, undef)
        # add type and value so we can use it as a token, too
        # (this just avoids a special case in the parser)
        self.type = self
        self.value = self
        
    def __repr__(self):
        return "null"
    def __str__(self):
        return "()"

# and it's a singleton:
null = null()


# * write 
##
## write takes an ecsydis data strucure and turns
## it back into a string
##

def write(data):
    s = StringIO()
    if listP(data):
        s.write("(")
        s.write(" ".join([write(child) for child in data]))
        s.write(")")
    else:    
        s.write(data)
    return s.getvalue()
