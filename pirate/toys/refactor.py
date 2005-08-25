from __future__ import generators
"""
From: Michal and Curt
To: pirate list


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

    def getLines(self):
        return self.lines
    
    def getCode(self):
        res  = ".sub __main__ @MAIN\n"
        res += "\n".join(list(self.getLines())) + "\n"
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
def simplify(tree):
    t = transform.Transformer()
    t.on(ast.Pass, replaceWith(PassEmitter))
    return t.apply(tree)



# Now we can delete PirateVisitor.visitPass
# because it will never see a "pass" node:
#
# (I'm using del for narrative reasons,
# but really we'd actually delete the 
# the old lines)
del PirateVisitor.visitPass


class PirateVisitor(PirateVisitor):

    def visitPassEmitter(self, node):
        self.lines.append(node.emit())



# Only one problem: node.emit() is a
# generator, not a string, and our lines
# need to be strings.
#
# But we can fix that:

    def getLines(self):
        return flatten(self.lines)
            
        
# flatten is just a simple function to iterate
# through the nested generators for us:

from types import GeneratorType

def flatten(series):
    """
    take a list that might contain other lists or
    generators  and flatten it so that it's just
    one big list
    """
    for thing in series:
        if type(thing) in [list, GeneratorType]:
            for sub in flatten(thing):
                yield sub
        else:
            yield thing

test()

"""
So now we can put one of our Emitter nodes in the tree, and
we can have pirate deal with it correctly. But what happens
if the Emitter node has children? Somehow, it needs to invoke
PirateVisitor.

Here's the problem:
"""


class PirateVisitor(PirateVisitor):

    def visitWhile(self, node):
        # *very* dumbed down: we're not even
        # actually putting the test in here!!!
        self.lines.append("while:")
        self.visit(node.body)
        self.lines.append("goto while")




def testWhile():
    pir = compile(simplify(parse("while 1: pass")))
    assert pir ==\
"""\
.sub __main__ @MAIN
while:
noop
goto while
.end
""", pir


testWhile()

"""
So far, this works fine. It's no problem to have an old
Node with an Emitter chld, but if we try this...
"""

def simplify(tree):
    t = transform.Transformer()
    t.on(ast.Pass,  replaceWith(PassEmitter))
    t.on(ast.While, replaceWith(WhileEmitter))
    return t.apply(tree)


del PirateVisitor.visitWhile

class PirateVisitor(PirateVisitor):
    def visitWhileEmitter(self, node):
        self.lines.append(node.emit())


"""
We run into a problem, because we've lost self.visit:
"""

class WhileEmitter(ast.While):
    def emit(self):
        yield "while:"
        self.visit(self.body) ## DOESN'T WORK ANYMORE
        yield "goto while"


"""
So, really what we need is to make a new call to
compile()... Except we only want the lines, not
the code with the full template... So we can
just break compile() in half:
"""

def emit(ast):
    vis = compiler.visitor.ASTVisitor()
    pir = PirateVisitor()
    vis.preorder(ast, pir)
    return pir

def compile(ast): # new version
    return emit(ast).getCode()

class WhileEmitter(ast.While):
    def emit(self):
        yield "while:"        
        yield emit(self.body).getLines()
        yield "goto while"


# now it works!
testWhile()


"""
That's basically it. PirateVisitor now looks like this:
"""

class PirateVisitor(object):

    def __init__(self):
        self.lines = []

    def getLines(self):
        return flatten(self.lines)
    
    def getCode(self):
        res  = ".sub __main__ @MAIN\n"
        res += "\n".join(list(self.getLines())) + "\n"
        res += ".end\n"
        return res

    ## our old style visitor methods:

    def visitPrintnl(self, node):
        self.lines.append('print_newline')


    # plus these visitXXXEmitter methods all of
    # which are EXACTLY the same.
    #
    # At some point, all the methods will look
    # like this, and we can replace PirateVisitor
    # with a simple dispatch dictionary.

    def visitPassEmitter(self, node):
        self.lines.append(node.emit())

    def visitWhileEmitter(self, node):
        self.lines.append(node.emit())


# and it still works!
test()
testWhile()


"""
From here on out, we just continue replacing the visitXXX
and expressXXX methods in this fashion.

Anyway, this is the proposal for the pirate refactoring.
Feedback, questions, and comments are appreciated. 
"""

