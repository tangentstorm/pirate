"""
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
how pirate works.
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
        self.lines.append('    print_newline')

    def visitPass(self, node):
        self.lines.append('    noop')

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
def compile(src):
    compiler.parse(src)
    ast = compiler.parse(src)
    vis = compiler.visitor.ASTVisitor()
    pir = PirateVisitor()
    vis.preorder(ast, pir)
    return pir.getCode()
    


## here's how it looks when it runs:

pir = compile("print; pass")
assert pir ==\
"""\
.sub __main__ @MAIN
    print_newline
    noop
.end
""", pir


##################################################

"""
Refactoring Plan
-------------------------------

The goal is to refactor pirate so that 

This is based on 'Phase Two: extract PIR related classes'
in this email:

http://cornerhost.com/archives/pirate/2005-July/000038.html

"""
