# introspection
from __future__ import generators
import compiler
from compiler.ast import Node
import sys

"""
this module contains basic stuff for transforming asts
"""

def visit(aNode, st=""):
    """
    this just serializes the tree into an indented list of strings
    """
    if (len(aNode.getChildNodes()) != 0):
        yield st + str(aNode.__class__)
        for child in aNode.getChildNodes():
            for line in visit(child, st+" "):
                yield line
                
    else:        
        yield st + str(aNode.__class__)      
                

def namedChildren(node):
    for name in dir(node):
        member = getattr(node, name)
        if isinstance(member, Node):
            yield Node, name, member
        elif isinstance(member, list):
            yield list, name, member
        else:
            pass
            #print name, member.__class__.__name__



def keepTheSame(node):
    return node

class Transformer:

    def __init__(self):
        self.dispatch = {}

    def on(self, onWhat, doWhat):
        """
        register a transformation (doWhat) for
        a particular class of node (onWhat)
        """
        self.dispatch[onWhat] = doWhat


    def apply(self, node):
        """
        apply our transformations to the node
        """        

        # first figure out what transformation to do:
        transform = self.dispatch.get(
            # it's based on the class
            node.__class__,
            # and by default we just return the node as-is
            keepTheSame)

        newNode = transform(node)

        # we're done with that old node for good now.
        # this just makes sure nothing touches it again:
        del node

        # now we still need to walk the tree, so handle all
        # the children of this new node recursively:
        for childType, name, child in namedChildren(newNode):
            # we can have individual nodes:
            if childType is Node:
                setattr(newNode, name, self.apply(child))
            # or lists of nodes:
            elif childType is list:
                setattr(newNode, name, [self.apply(item) for item in child])

        # return the transformed tree:
        return newNode


def copyTree(tree):
    return Transformer().apply(tree)


if __name__=="__main__":

    # example code
    # @TODO: put this under real tests... :)
    
    if len(sys.argv) > 1:
        source = sys.argv[1]
    else:
        source = "tradComp.py"
        
    tree = compiler.parseFile(source)

    assert list(visit(tree)) == list(visit(copyTree(tree)))
    print "same!"

##########################
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
        
        def __init__(self, decorators, name, argnames, defaults, flags, doc, code):
            self.decorators = decorators
            self.name = "\'" + str(name) + "\'"
            self.argnames = argnames
            self.defaults = defaults
            self.flags = flags
            self.doc = doc
            self.code = code
            
        def __repr__(self):
            return "CurtsGenerator!!(%s, %s, %s, %s, %s, %s, %s)" % \
            (self.decorators, self.name, self.argnames,\
            self.defaults, self.flags, self.doc, self.code)
            
        
    def convertFunction(node):
        return Generator( node.decorators, node.name, 
            node.argnames, node.defaults, node.flags, 
            node.doc, node.code)
        
    #print tree    
    t = Transformer()
    t.on(Discard, expressListComp)
    t.on(ListComp, expressListComp)
    t.on(Function, convertFunction)
    t.apply(tree)
    #print t.apply(tree)
    #tree.filename = ""
    #gen = pycodegen.ModuleCodeGenerator(tree)
    #code = gen.getCode()
    #exec code
    