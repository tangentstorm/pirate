#!/usr/bin/python

import os
import compiler
from compiler import pycodegen
import traceback
from compiler import ast
from compiler.visitor import *
import sys

MyTree = compiler.parseFile(sys.argv[1])


class SimpVisitor(object):
    
    def convertListComp(self, node):
        #Not fully implemented yet
        # i need to take the node.expr and append at the end
        # i need to take node.quals have a For be in place of the listcomp 
        length = len(node.quals)
        for i in range(length):
            if str(node.quals[i].__class__.__name__) == 'ListCompFor':
                head = node.quals[i].assign
                list = node.quals[i].list
                ifs = node.quals[i].ifs
                if ifs[0] is not None: 
                    newIf = self.getIf(ifs[0])
                node.quals[i] = compiler.ast.For(head,
                           list,
                           newIf,
                           else_ = None)
                #print node.quals[i]           
            else:
                pass            
                           
            
    def getIf(self, node):        
        return compiler.ast.If(node.test, None)
        

    def findReturn(self, node):
        for child in node.getChildNodes():
            #Stmt Holds the return statement if there is one.
            if str(child.__class__.__name__) == 'Stmt':
                length = len(child.nodes)
                if(str(child.nodes[length-1].__class__.__name__) == 'Return'):
                    #Uncomment the 2 lines below to watch the return 
                    #being changed to a yield as it walks
                    #print child#before the conversion to yield
                    val = child.nodes[length-1].value
                    child.nodes[length-1] = compiler.ast.Yield(val)
                    #print child#after the conversion to yield
            ##If there are more If's etc, then keep digging
            for kid in child.getChildNodes():
                if str(kid.__class__.__name__) == 'If' or 'While' or 'For':
                    self.findReturn(kid)
                
    def convertFunction(self, node):  # visitDef
        
        #node = compiler.ast.GenExpr(node.code)
        ##This handles the very last Return statement in a Function
        length = len(node.code.nodes)
        if str(node.code.nodes[length-1].__class__.__name__) == 'Return':
            val = node.code.nodes[length-1].value
            node.code.nodes[length-1] = compiler.ast.Yield(val)
        
        #This passes nested returns to a helper method            
        for child in node.code.nodes:
            className = child.__class__.__name__
            if className == 'If' or 'While' or 'For':
                self.findReturn(child)
           
        return node

## MyASTVisitor ###################################################

class MyASTVisitor(ASTVisitor):  
        
    def dispatch(self, node, *args):
        className = str(node.__class__.__name__)
        if(className == "ListComp"):
            #print "*"*12+str(node)
            self.visitor.convertListComp(node)   
            #print "&"*12+str(node)
        if(className == "Function"):
             node = compiler.ast.GenExpr(node.code)
             print "*"*12 + str(node)
             self.visitor.convertFunction(node)   
             print "&"*12 + str(node)
##Below this is default visitor.py method dispatch
        self.node = node
        klass = node.__class__
        meth = self._cache.get(klass, None)
        if meth is None:
            className = klass.__name__
            meth = getattr(self.visitor, 'visit' + className, self.default)
            self._cache[klass] = meth

        return meth(node, *args)

def doit():
    
    vis = MyASTVisitor()
    pir = SimpVisitor()
    # Prints the tree before and after simplification
    print MyTree
    vis.preorder(MyTree, pir)
    print MyTree
    #MyTree.filename = ""
    #gen = pycodegen.ModuleCodeGenerator(MyTree)
    #code = gen.getCode()
    #exec code

doit()