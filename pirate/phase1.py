"""
this is curt's initial version of simple.py, as
extracted from transform.py
"""
from transform import Transformer
from compiler.ast import ListComp
from compiler.ast import Discard
from compiler.ast import Function
from compiler import ast
from compiler import pycodegen


global ListCompExpr    
class ListCompExpr(ast.Node):
    def __init__(self, expr):
        self.expr = expr
# takes in the Discard or listComp node and
# sends the appropriate body to comprehend
# to break down the listcomp
def expressListComp(node):
    # we were given a Discard node
    if isinstance(node.expr, ListComp):
        queue = node.expr.quals + [ListCompExpr(node.expr.expr)]
        return comprehend(0, queue)
    # we were given a ListComp node    
    elif isinstance(node, ListComp):
        queue = node.quals + [ListCompExpr(node.expr)]
        return comprehend(1, queue) 
    else:
        return node
# We pass comprehend a 0 if the ListComp is surrounded 
# by a Stmt node, and 1 if it is not. We know this
# because a Discard is wrapped in a Stmt, and a print
# call before a ListComp is not wrapped in a Stmt node.
#
# This should return a tree that is syntactically identical
# to a ListComp translated into a for/if AST.
#
def comprehend(int, queue):
    """
    do our own walk of the tree and rebuild
    using ast.For and ast.If
    """
    head, tail = queue[0], queue[1:]
    if int == 0:# it was a Discard node
        if isinstance(head, ast.ListCompFor):
            return ast.For(assign = head.assign,
                           list = head.list,
                           body = comprehend(1, head.ifs + tail),
                           else_ = None)                         
    else:# it was a print node or subsequent calls to comprehend
        head, tail = queue[0], queue[1:]
        if isinstance(head, ast.ListCompFor):
            return ast.Stmt([ast.For(assign = head.assign,
                           list = head.list,
                           body = comprehend(1, head.ifs + tail),
                           else_ = None)])                         

        elif isinstance(head, ast.ListCompIf):
            return ast.Stmt([ast.If(tests = [(head.test, comprehend(1, tail))],
                          else_ = None)])
        elif isinstance(head, ListCompExpr):
            return ast.Stmt([Discard(head.expr)])
        else:
            raise "i can't comprehend a %s" % head.__class__.__name__

# HELP!!! This doesn't look right.
class Generator(Function):        

    def __init__(self, name, argnames, defaults, flags, doc, code):
       # self.decorators = decorators
        self.name = "\'" + str(name) + "\'"
        self.argnames = argnames
        self.defaults = defaults
        self.flags = flags
        self.doc = doc
        self.code = code

    def __repr__(self):
        return "Function(%s, %s, %s, %s, %s, %s)" % \
        (self.name, self.argnames,\
        self.defaults, self.flags, self.doc, self.code)


def convertFunction(node):
    return Generator(node.name, 
        node.argnames, node.defaults, node.flags, 
        node.doc, node.code)


if __name__=="__main__":
    ## this stuff is all test code
    ## so by convention we put it in one of these blocks
    t = Transformer()
    t.on(Discard, expressListComp)
    t.on(ListComp, expressListComp)
    t.on(Function, convertFunction)
    #newTree = t.apply(tree)
    #print t.apply(tree)
    #tree.filename = ""
    #gen = pycodegen.ModuleCodeGenerator(tree)
    #code = gen.getCode()
    #exec code

    
