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

