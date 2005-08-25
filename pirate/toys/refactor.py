from __future__ import generators

"""
From: Michal
To: Curt, pirate list


Refactoring Pirate : an example
-------------------------------

Our goal is to refactor pirate so that compilation
happens in several passes: a series of tree trans
formations, which results in a final tree of 
PIR-centric nodes.
"""

##################################################

"""
Current situation
-------------------------------

Here is a GREALY simplified example of 
how pirate works as of today (Aug 24, 2005):
"""

## PirateVisitor walks the abstract syntax tree
## created by python's compiler package, and
## compiles the tree into parrot intermediate
## representation (PIR)
##
## (Actually, we simplify the AST somewhat in
## simple.py but it's still a tree of nodes)

class PirateVisitor(object):

    ## it holds a list of emitted PIR lines
    def __init__(self):
        self.lines = []

    ## As we visit each node in the syntax tree,
    ## we add some bytecode to the list.
    ##
    ## Here are simple examples of how we might
    ## implement the bare "print" and "pass"
    ## keywords
    ##
    ## these method names correspond to nodes
    ## defined in the compiler/ast.py module

    def visitPrintnl(self, node):
        self.lines.append('print_newline')

    def visitPass(self, node):
        self.lines.append('noop')

    ## when we're done, the whole thing is wrapped
    ## in a template:
    ##
    ## (again, real pirate is much more complicated)
    
    def getCode(self):
        res  = ".sub __main__ @MAIN\n"
        res += "\n".join(self.lines) + "\n"
        res += ".end\n"
        return res


## now we can use the compiler module to drive
## our visitor and compile the source:

import compiler
def parse(src):
    return compiler.parse(src)

def simplify(ast):
    return ast # nothing yet

def compile(ast):
    vis = compiler.visitor.ASTVisitor()
    pir = PirateVisitor()
    vis.preorder(ast, pir)
    return pir.getCode()
    


## here's how it looks when it runs:

def test():
    pir = compile(simplify(parse("print; pass")))
    assert pir ==\
"""\
.sub __main__ @MAIN
print_newline
noop
.end
""", pir

test()

##################################################

"""
Refactoring Plan
-------------------------------

The goal is to refactor pirate so that the entire compilation
process happens as a series of tree transformations, right up
until the end, so that we wind up with a tree that looks very
much like an AST for PIR.

Given the time constraints for the summer of code sponsorship,
I suggest we focus our energies on an intermediate step:
extracting individual classes for each visitXXX and expressXXX
method.

The result is that each Node from compiler.ast (or at
least the ones we haven't simplified out) should have
a corresponding NodeEmitter class:

For example, here's how we can implement "pass":
"""

from compiler import ast
class PassEmitter(ast.Pass):
    def emit(self):
        yield "noop"
    


"""
In other words, we're creating a new type of node that
looks just like Pass, but also has an .emit() method.

It's easy to do a search and replace on these in our
ast, now that we have the transform module:

"""

# this returns a transformer function
# we can use this over and over again
def replaceWith(newClass):
    def func(node):
        node.__class__ = newClass
        return node
    return func
    

# now override the simplify method by adding
# some transformations.
#
# (this new version will be called next time
# we run test())
import transform
def simplify(ast):
    t = Transformer()
    t.on(ast.Pass, replaceWith(PassEmitter))
    return t.apply(ast)



# Now we can delete PirateVisitor.visitPass
# because it will never see a "pass" node:
#
# (I'm using del for narrative reasons,
# but really we'd actually delete the 
# the old lines)
del PirateVisitor.visitPass


class PirateVisitor(PirateVisitor):

    def visitPass(self, node):
        emitter = PassEmitter(node)
        self.lines.append(emit.emitter())





"""

So what we're doing here is pretty simple in
principle, but how can we mix this style of
class with the 


Now: how can we tie this scheme into pirate without
breaking all the tests?

"""



