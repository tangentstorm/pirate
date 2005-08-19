import compiler
import sys

myAST = compiler.ast

def visit(aNode, st=""):          
    if (len(aNode.getChildNodes()) != 0):
        print st + str(aNode.__class__)

        for child in aNode.getChildNodes():    
            visit(child, st+"\t")        
    else:
        print "elsed" + st + str(aNode.__class__)      
        
        
tree = compiler.parseFile(sys.argv[1])
visit(tree) 
