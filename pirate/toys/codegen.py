#!/usr/bin/python
"""
takes s-expressions on stdin and
translates it to PIR on stdout.
"""
import unittest
import readlisp

def parse(code):
    return readlisp.readlisp(code)

def compile(tree):
    return eval("on_%s(*tree[1:])" % tree[0])

def pandc(code):
    return compile(parse(code))
    
def on_assign(left, right):
    var = compile(left)
    val = compile(right)
    return [".local int %s" % var,
            "%s = %s" % (var, val)]

def on_begin(*nodes):
    res = []
    for node in nodes:
        res.extend(compile(node))
    return res

def on_name(node):
    return str(node)

def on_const(node):
    return str(node)

def on_pass():
    return ["noop"]

def on_print(what):
    return ["print %s" % compile(what)]

def on_while(cond, body):
    return While(cond, body).generate()

class While:
    def __init__(self, cond, body):
        self.cond = cond
        self.body = body
        self.lines = []
        
    def generate(self):
        self.lines.append("L1:")
        self.lines.append("$I0 = %s" % compile(self.cond))
        self.lines.append("unless $I0, L2")
        self.lines.extend(compile(self.body))
        self.lines.append("branch L1")
        self.else_stub()        
        self.lines.append("L2:")
        return self.lines

    def else_stub(self):
        """
        this space reserved for python's else clause
        """
        
class PythonWhile(While):
    def __init__(self, cond, body, else_):
        super(PythonWhile, self).__init__(cond,body)
        self.else_ = else_
    def else_stub(self):
        self.lines.append("#ELSE goes here")
    

class CodeGenTest(unittest.TestCase):
    def testName(self):
        self.assertEquals(pandc("(name x)"), "x")

    def testPass(self):
        self.assertEquals(pandc("(pass)"),
                          ["noop"])
    def testPassTwice(self):
        self.assertEquals(pandc("(begin (pass) (pass))"),
                          ["noop",
                           "noop"])
        
    def testAssign(self):
        self.assertEquals(pandc("(assign (name x) (const 1))"),
                          [".local int x",
                           "x = 1"])

    def testWhile(self):
        self.assertEquals(pandc("(while (const 1) (pass))"),
                          ["L1:",
                           "$I0 = 1",         # <- const 1
                           "unless $I0, L2",
                           "noop",
                           "branch L1",
                           "L2:"])

if __name__=="__main__":
    import sys
    import os
    if "-t" in sys.argv:
        sys.argv = sys.argv[:1] # evil!
        unittest.main()
    else:
        ast = sys.stdin.read()
        imc = "\n".join(pandc(ast))
        print imc
