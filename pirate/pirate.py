#!/usr/bin/python
"""
pirate: python->parrot compiler

loosely based on parrot-gen.py by amk:
http://www.amk.ca/conceit/parrot.html

"""
__doc__=\
"""
pirate: python to parrot compiler 

usage:  pirate.py [-d] filename.py
        -d dumps the generated parrot code
"""

import os
import compiler
import traceback
from compiler import ast        

class imclist(list):
    """
    a list containing one imc instruction per line
    """
    def append(self, line):
        if ':' not in line:
            line = "    %-30s"  %line
            if "#" not in line:
                line = line + "# %s"  % self.find_linenumber()
        super(imclist, self).append(line)
    def find_linenumber(self):
        """
        python black magic. :)
        """
        stack = traceback.extract_stack()        
        file, line, func = stack[-3][:3]
        if func == "append":
            file, line, func = stack[-4][:3]
        return "(%s:%s)" % (func,line)


class PirateVisitor(object):

    ##[ management stuff ]##########################################
    
    def __init__(self, name):
        self.name = name
        self._last_lineno = None
        self.lines = imclist()
        self.loops = []
        self.counter = {}
        self.subs = []
        self.vars = {}

    def symbol(self, prefix):
        """
        Return a unique symbol for label names in generated code
        """
        self.counter.setdefault(prefix,0)
        self.counter[prefix] += 1
        return "%s%05i" % (prefix, self.counter[prefix])

    def getCode(self):
        res  = ".sub %s\n" % self.name
        res += "\n".join(self.lines) + "\n"
        res += ".end\n"
        res += ".include 'pirate.imc'"
        res += "\n".join([s.getCode() for s in self.subs]) + "\n"
        return res
    

    def set_lineno (self, node):
        if (node.lineno is not None and
            node.lineno != self._last_lineno):
            self._last_lineno = node.lineno
            res = imclist()
            res.append('setline %i' % node.lineno)
            return res
        else:
            return []

    def append(self, line):
        self.lines.append(line)


    def unappend(self):
        self.lines.pop()
        

    ##[ expression compiler ]######################################
    
    def compileExpression(self, node, dest):
        """
        In CPython, expression results just get pushed
        onto the stack. But since parrot uses registers
        instead, we need to to specify a destination 
        for each expression, and so we have to do our
        own dispatching outside the normal visitor walk.
        """

        # in a sub, statements seem to get wrapped in
        # Stmt. skip it:
        if isinstance(node, ast.Stmt):
            self.visit(node)
            return []
        
        handler = {
            ast.Name:     self.nameExpression,
            ast.Const:    self.constantExpression,
            ast.List:     self.listExpression,
            ast.Lambda:   self.lambdaExpression,
            ast.CallFunc: self.callingExpression,
            ast.Compare:  self.compareExpression,
            
            ast.Or:  self.logicExpression,
            ast.And: self.logicExpression,
            ast.Not: self.logicExpression,
            
            ast.Add: self.infixExpression,
            ast.Sub: self.infixExpression,
            ast.Mul: self.infixExpression,
            ast.Div: self.infixExpression,
            ast.Mod: self.infixExpression,

            # leo's patch:
            #ast.Sub: self.binaryExpression,
        }
        try:
            return handler[node.__class__](node, dest)
        except KeyError:
            print "## unknown expression:"
            print node
            print "## entering debugger..."
            print
            import pdb; pdb.set_trace()


    constMap = {
        str: "PerlString",
        int: "PerlInt",
    }


    def constantExpression(self, expr, dest):
        t = type(expr.value)
        assert t in self.constMap, "unsupported const type:%s" % t
        res = self #imclist()
        res.append("%s = new %s" % (dest, self.constMap[t]))
        res.append("%s = %s" % (dest, repr(expr.value)))
        return []


    def nameExpression(self, expr, dest):
        res = self #imclist()
        res.append("%s = %s" % (dest, expr.name))
        return []#res


    def listExpression(self, expr, dest):
        self.append("%s = new PerlArray" % dest)
        sym = self.symbol("$P")
        for item in expr.nodes:
            self.compileExpression(item, sym)
            self.append("push %s, %s" % (dest, sym))
        return []
        
    typemap = {
	str: "S",
	int: "I",
	float: "N"
    }
    def binaryExpression(self, node, dest):
        op = self.infixOps[node.__class__]
        
        # special case for constants
        if isinstance(node.left, ast.Const) \
        and isinstance(node.right, ast.Const):
	    if type(node.left.value) == type(node.right.value):
		sym = self.symbol("$%s" % self.typemap[type(node.left.value)])
	    else:
		tl = self.typemap[type(node.left.value)]
		tr = self.typemap[type(node.right.value)]
		if tl == "S" and tr == "S" and op == "+":
		    sym = "TODO concat"
		if tl == "N" or tr == "N":
		    sym = self.symbol("$N")
	    lsym = self.compileExpression(node.left,dest)
	    rsym = self.compileExpression(node.right,dest)

        # at least one non-constant
	else:
	    lexpr = self.compileExpression(node.left, dest)            
	    if isinstance(node.left, ast.Const) or not lexpr.find("$P") == 0:
		lsym = self.symbol("$P")
		self.append("%s = new PerlUndef" % lsym)
		self.append("%s = %s #lc" % (lsym, lexpr))
	    else:
		lsym = lexpr
	    rexpr = self.compileExpression(node.right,dest)
	    if isinstance(node.right, ast.Const) or not rexpr.find("$P") == 0:
		rsym = self.symbol("$P")
		self.append("%s = new PerlUndef" % rsym)
		self.append("%s = %s #rc" % (rsym, rexpr))
	    else:
		rsym = repxr
	    sym = self.symbol("$P")
	    self.append("%s = new PerlUndef" % sym)
	self.append("%s = %s %s %s" % (sym, lsym, op, rsym))
	return ["%s = %s" % (dest, sym)]




    infixOps = {
        ast.Add: "+",
        ast.Sub: "-",
        ast.Mul: "*",
        ast.Div: "/",
        ast.Mod: "%",
        #ast.Power: "**",        # doesn't work yet
        #ast.RightShift: '>>',   # untested
        #ast.LeftShift: '<<',    # untested
    }

        
    def infixExpression(self, expr, dest):
        operator = self.infixOps[expr.__class__]
        symleft,  typleft  = self.symbol("$P"), "PerlInt"
        symright, typright = self.symbol("$P"), "PerlInt"
        symexpr,  typexpr  = self.symbol("$P"), "PerlInt"
        res = self #imclist()

        # store left side of expression in symleft:
        res.append("%s = new %s" % (symleft, typleft))
        self.compileExpression(expr.left, symleft)

        # store right side of expression in symright:
        res.append("%s = new %s" % (symright, typright))
        self.compileExpression(expr.right, symright)

        # store the combined value in symexpr
        res.append("%s = new %s" % (symexpr, typexpr))
        res.append("%s = %s %s %s" \
                   % (symexpr, symleft, operator, symright))

        # and finally put the result in our destination
        # (imcc seems to like this as a separate step for
        # typecasting or something...)
        res.append("%s = %s" % (dest, symexpr))

        return []
    

    def compareExpression(self, expr, dest):
        assert len(expr.ops) == 1, "multi-compare not working yet"
        res = self#imclist()       
        # get left side:
        symL = self.symbol("$P")
        self.compileExpression(expr.expr, symL)

        # get the op:
        op, code = expr.ops[0]
        if op=="<>": op="!="
        
        # get right side:
        symR = self.symbol("$P")
        self.compileExpression(code, symR)

        _cmp = self.symbol("cmp")
        _end = self.symbol("endcmp")
        res.append("%s = new PerlInt" % dest)
        res.append("if %s %s %s goto %s" % (symL, op, symR, _cmp))
        res.append("%s = 0" % dest)
        res.append("goto %s" % _end)
        res.append("%s:" % _cmp)
        res.append("%s = 1" % dest)
        res.append("%s:" % _end)
        return []


    logicOps = {
        ast.Not: '!',
        ast.And: 'and',
        ast.Or: 'or',
    }


    def logicExpression(self, expr, dest):
        operator = self.logicOps[expr.__class__]
        res = self#imclist()
        if operator == "!":            
            self.compileExpression(expr.expr, dest)
            res.append("not %s, %s" % (dest,dest))
        else:
            L,R = expr.nodes
            tmp = self.symbol("tmp")
            res.append(".local PerlInt %s" % tmp)
            self.compileExpression(L, dest)
            self.compileExpression(R, tmp)
            res.append("%s %s, %s, %s" % (operator, dest, dest, tmp))
        return []


    def callingExpression(self, node, dest):
        assert not (node.star_args or node.dstar_args), \
               "f(*x,**y) not working yet"
        
        res = self
        args = []
        node.args.reverse()
        for arg in node.args:
            var = self.symbol("arg")
            res.append(".local object %s" % var)
            self.compileExpression(arg, var)
            args.append(".arg %s" % var)

        # figure out what we're calling
        adr = self.symbol("$I")        
        if isinstance(node.node, ast.Lambda):
            # lambdas don't have names!
            self.lambdaExpression(node.node, adr, allocate=0)
        else:
            sub = node.node.name
            res.append("%s = %s" % (adr, sub))

        sub_pmc = self.symbol("$P")
        ret = self.symbol("returnaddr")
        ret_pmc = self.symbol("$P")

        ## now call it:

        # @TODO: newsub op not working for me yet
        res.append("%s = new Sub" % sub_pmc)
        res.append("%s = %s" % (sub_pmc, adr))
        res.append("%s = new Continuation" % ret_pmc)
        cadr = self.symbol("$I")
        res.append("%s = addr %s" % (cadr, ret))
        res.append("%s = %s" % (ret_pmc, cadr))
                
        res.append(".pcc_begin non_prototyped")
        for r in args:
            res.append(r)
        res.append('.pcc_call %s, %s' % (sub_pmc, ret_pmc))
        res.append('%s:' % ret)
        if dest:
            res.append(".result %s" % dest)
        res.append(".pcc_end")
        return []


    def lambdaExpression(self, node, dest, allocate=1):
        return self.genFunction(node,dest,allocate)


    def genFunction(self, node, dest, allocate=1):
        assert not node.kwargs or node.varargs, "only simple args for now"
        self.set_lineno(node)

        # anonymous function, so no name
        sub = self.symbol("_sub")
        adr = self.symbol("$I")

        # fork a new code generator to walk the function's tree:
        vis = compiler.visitor.ASTVisitor()
        pir = PirateSubVisitor(sub,
                               doc="lambda from line %s" % node.lineno,
                               args=node.argnames)
        vis.preorder(ast.Return(node.code), pir)
        self.subs.append(pir)

        # store the address in dest
        self.append("%s = addr %s" % (adr, sub))
        if allocate:
            self.append("%s = new PerlInt" % dest)
        self.append("%s = %s" % (dest, adr))
        return []



    ##[ visitor methods ]##########################################
        
    def visitPrint(self, node):
        assert node.dest is None, "print >> not yet handled"
        for n in node.nodes:
            self.set_lineno(n)
            dest = self.symbol("$P")
            self.compileExpression(n, dest)
            self.append('.arg %s' % dest)
            self.append('call __py__print')
            self.append('print " "')


    def visitPrintnl(self, node):
        self.visitPrint(node)
        if node.nodes:
            self.unappend() # remove final space
        self.append('print "\\n"')


    def visitIf(self, node):
        _endif = self.symbol("endif")
        
        for test, body in node.tests:

            # if not true, goto _elif
            self.set_lineno(test)
            _elif = self.symbol("elif")
            testvar = self.symbol("test")

            self.append(".local object %s" % testvar)
            self.append("%s = new PerlInt" % testvar)
            self.compileExpression(test, testvar)
            self.append("unless %s goto %s" % (testvar, _elif))
            
            # do it and goto _endif
            self.visit(body)
            self.append("goto %s" % _endif)
            
            # _elif: (next test or pass through to else)
            self.append("%s:" % _elif)

        # else:
        if node.else_:
            self.set_lineno(node.else_)
            self.visit(node.else_)
            
        # _endif:
        self.append("%s:" % _endif)


    def visitAssign(self, node):
        leftside = node.nodes[0]
        rightside = node.expr
        if isinstance(leftside, ast.AssTuple):
            leftside = leftside.nodes
            rightside = rightside.nodes
        else:
            leftside = [leftside]
            rightside = [rightside]
        for node, expr in zip(leftside, rightside):
            name = node.name
            self.append(".local object %s" % name)
            self.compileExpression(expr, name)


    def visitWhile(self, node):
        assert node.else_ is None, "while...else not supported"
        self.set_lineno(node)
        _while = self.symbol("while")
        _endwhile = self.symbol("endwhile")
        self.loops.append((_while, _endwhile))
        self.append("%s:" % _while)
        testvar = self.symbol("$P")
        self.append("%s = new PerlInt" % testvar)
        self.compileExpression(node.test, testvar)
        self.append("unless %s goto %s" % (testvar, _endwhile))
        self.visit(node.body)
        self.append("goto %s" % _while)
        self.append("%s:" % _endwhile)
        self.loops.pop()


    def visitFor(self, node):
        assert node.else_ is None, "for...else not supported"
        assert not isinstance(node.assign, ast.AssTuple), \
               "for x,y not implemented yet"

        self.set_lineno(node)
        self.append(".local object %s" % node.assign.name)
        _for = self.symbol("for")
        _endfor = self.symbol("endfor")
        loopidx = self.symbol("idx")
        forlist = self.symbol("list")
        listlen = self.symbol("$I")

        self.loops.append((_for, _endfor))

        # first get the list
        self.append(".local PerlArray %s" % forlist)
        self.compileExpression(node.list, forlist)

        # forlist = len(list)
        self.append("%s = %s" % (listlen, forlist))

        # int counter = 0
        self.append(".local int %s" % loopidx)        
        self.append("%s = 0" % loopidx)

        # get the next item (also where "continue" jumps to)
        self.append("%s:" % _for)
        self.append("%s = %s[%s]" % (node.assign.name, forlist, loopidx))
        self.append("%s = %s + 1" % (loopidx, loopidx))
        
        # do the loop body
        self.visit(node.body)

        # now loop!
        self.append("if %s < %s goto %s" % (loopidx, listlen, _for))
        self.append("%s:" % _endfor)
        
        self.loops.pop()


    def visitBreak(self, node):
        assert self.loops, "break outside of loop" # SyntaxError

        self.append("goto %s" % self.loops[-1][1])


    def visitContinue(self, node):
        assert self.loops, "continue outside of loop" # SyntaxError
        self.append("goto %s" % self.loops[-1][0])


    def visitCallFunc(self, node):
        # visited when a function is called as a subroutine
        # (not as part of a larger expression or assignment)
        self.callingExpression(node, dest=None)

    def visitReturn(self, node):
        _res= self.symbol("res")
        self.append(".local object %s" % _res)
        self.compileExpression(node.value, _res)
        self.append(".pcc_begin_return")
        self.append(".return %s" % _res)
        self.append(".pcc_end_return")

    def visitPass(self, node):
        self.append("noop")

    def visitFunction(self, node):  # visitDef
        self.append(".local object %s" % node.name) #@TODO: use set_lex
        self.genFunction(node, node.name, allocate=1)


                


class PirateSubVisitor(PirateVisitor):
    """
    I am just like the normal visitor, but
    work on subroutines instead of whole
    programs.
    """
    def __init__(self, name, doc, args=[]):
        super(PirateSubVisitor, self).__init__(name)
        self.doc = doc
        self.args = args
    def getCode(self):
        res = ""
        if self.doc:
            res += "# %s\n" % self.doc
        res  += ".pcc_sub %s non_prototyped\n" % self.name
        for arg in self.args:
            res += "    .param object %s\n" % arg
        res += "\n".join(self.lines) + "\n"
        res += ".end\n"
        return res
        
## module interface ###############################################

def compile(src):
    ast = compiler.parse(src)
    vis = compiler.visitor.ASTVisitor()
    pir = PirateVisitor("__main__")
    vis.preorder(ast, pir)    
    pir.append("end")
    return pir.getCode()
    

def line_nos(seq):
    return [(i+1, seq[i]) for i in range(len(seq))]

def invoke(src, dump=0, lines=0):
    i,o = os.popen4("imcc -")
    code = compile(src)
    if dump:
        print
        if lines:
            for no, line in line_nos(code.split("\n")):
                print "% 4i: %s" % (no, line)
        else:
            print code
    print >> i, code
    i.close()    
    return o.read()

if __name__=="__main__":
    import sys
    if len(sys.argv) > 1:
        # file or stdin?
	if sys.argv[-1] == '-':
	    src = sys.stdin.read()
	else:
	    src = open(sys.argv[-1]).read()
        # dump or run?
        if "-d" in sys.argv:
            print compile(src)
        else:
            sys.stdout.write(invoke(src))
    else:
        print __doc__
        sys.exit()
        
