#!/usr/bin/python
"""
pirate: python->parrot compiler

loosely based on parrot-gen.py by amk:
http://www.amk.ca/conceit/parrot.html

"""
import os
import compiler

class PirateVisitor:

    ##[ management stuff ]##########################################
    
    def __init__(self):
        self._last_lineno = None
        self.lines = ["__main__:"]

    def set_lineno (self, node):
        if (node.lineno is not None and
            node.lineno != self._last_lineno):
            self._last_lineno = node.lineno
            return ['setline %i' % node.lineno]
        else:
            return []

    def append(self, line):
        if ':' not in line:
            line = '    ' + line
        self.lines.append(line)

    def unappend(self):
        self.lines.pop()
        
    def extend(self, lines):
        for line in lines:
            self.append(line)
            
    ##[ visitor methods ]##########################################
        
    def visitPrintnl(self, node):
        assert node.dest is None, "print >> not yet handled"
        for n in node.nodes:
            self.extend(self.set_lineno(n))
            self.append('print %s' % repr(n.value))
            self.append('print " "')
        if node.nodes:
            self.unappend() # remove final space
        self.append('print "\\n"')


    def visitIf(self, node):
        assert len(node.tests) == 1, "Only one test supported"
        testExpr, body = node.tests[0]
        self.extend(self.set_lineno(testExpr))
        self.append("unless %s goto _else" % testExpr.value)
        self.visit(body)
        self.append("goto _endif")
        self.append("_else:")
        self.visit(node.else_)
        self.append("_endif:")

## module interface ###############################################

def compile(src):
    ast = compiler.parse(src)
    vis = compiler.visitor.ASTVisitor()
    pir = PirateVisitor()
    vis.preorder(ast, pir)
    pir.append("end")
    return ("\n".join(pir.lines)) + "\n"
    

def invoke(src, dump=0):
    i,o = os.popen4("imcc -")
    code = compile(src)
    if dump:
        print code
    print >> i, code
    i.close()    
    return o.read()

