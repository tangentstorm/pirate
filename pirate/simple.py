"""
python simplifier

does tree transformations on the python AST
to make pirate's job easier later on.

  - turns list comprehensions into for/ifs
  - (eventually) pre-categorizes functions and generators
  
"""
from compiler import ast


### list comprehensions ##############################

## This section transforms a list comprehension into a
## set of nested for and if blocks.


class SimpleListComp(ast.Node):
    def __init__(self, forLoop):
        # the big papa for loop in the list comp
        self.forLoop = forLoop

    def getChildNodes(self):
        return [self.forLoop]


class ListCompCore(ast.Node):
    def __init__(self, expr):
        self.expr = expr


def comprehend(queue):
    """
    do our own walk of the tree and rebuild
    using ast.For and ast.If
    """
    head, tail = queue[0], queue[1:]
    if isinstance(head, ast.ListCompFor):
        return ast.For(assign = head.assign,
                       list = head.list,
                       body = comprehend(head.ifs + tail),
                       else_ = None)                         
    elif isinstance(head, ast.ListCompIf):
        return ast.If(tests = [(head.test, comprehend(tail))],
                      else_ = None)
    elif isinstance(head, ListCompCore):
        return head
    else:
        raise "i can't comprehend a %s" % head.__class__.__name__


def simplifyListComp(node):
    queue = node.quals + [ListCompCore(node.expr)]
    return SimpleListComp(comprehend(queue))


