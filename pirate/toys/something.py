"""
this is an example that illustrated how a refactoring
to separate the emitter(s) from the parser might look
"""

from compiler.visitor import *
import pirate

class Something(object):
    def getChildNodes(self):
        return []

    def emit(self):
        print "hello!"

class Magic(object):
    def getChildNodes(self):
        return []

    def emit(self):
        print "magic!"

class BigTree(object):
    def getChildNodes(self):
        return [Something(), Something(), Magic()]

    def emit(self):
        print "big tree!"
    

class SpecialVisitor(pirate.PirateVisitor):
    pass

#    def visitSomething(self, node):
#        node.emit()
#
#    def visitMagic(self, node):
#        node.emit()


class MyASTVisitor(ASTVisitor):

    def dispatch(self, node, *args):
        ASTVisitor.dispatch(self, node, *args)
        node.emit()

    
vis = MyASTVisitor()
vis.preorder(BigTree(), SpecialVisitor(name="Fred"))
