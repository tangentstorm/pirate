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

class PirateVisitor:

    ##[ management stuff ]##########################################
    
    def __init__(self, name, doc="", args=[]):
        self.name = name
        self.doc = doc
        self.args = args
        self._last_lineno = None
        self.lines = []
        self.loops = []
        self.counter = {}
        self.subs = []

    def symbol(self, prefix):
        """
        Return a unique symbol for label names in generated code
        """
        self.counter.setdefault(prefix,0)
        self.counter[prefix] += 1
        return "%s%05i" % (prefix, self.counter[prefix])

    def getCode(self):
        res  = ".sub %s\n" % self.name
        if self.doc:
            res += "    # %s\n" % self.doc
        if self.args:
            res += "    saveall\n"
        for arg in self.args:
            res += "    .param object %s\n" % arg
        res += "\n".join(self.lines) + "\n"
        res += ".end\n" 
        res += "\n".join([s.getCode() for s in self.subs]) + "\n"
        return res
    

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

    ##[ expression compiler ]######################################
    
    typeMap = {
        str: "PerlString",
        int: "PerlInt",
    }

    def expression(self, expr, dest):
        """
        create code to eval expression Node 'expr' and put it in 'dest'
        """
        klass = expr.__class__
        
        ## plain old variables
        if klass==compiler.ast.Name:
            return ["%s = %s" % (dest, expr.name)]

        ## constants
        elif klass==compiler.ast.Const:
            t = type(expr.value)
            assert t in self.typeMap, "unsupported const type:%s" % t
            return [("%s = new %s" % (dest, self.typeMap[t])),
                    ("%s = %s" % (dest, repr(expr.value)))]

        ## lists
        elif klass==compiler.ast.List:
            res = ["%s = new PerlArray" % dest]
            sym = self.symbol("$P")
            for item in expr.nodes:
                res.extend(self.expression(item, sym))
                res.append("push %s, %s" % (dest, sym))
            return res
        
        ## math expressions
        elif klass in self.infixOps:
            return self.infixExpression(expr, dest)

        ## comparisons
        elif klass==compiler.ast.Compare:
            return self.compareExpression(expr, dest)

        ## boolean logic
        elif klass in self.logicOps:
            return self.logicExpression(expr, dest)

        ## function call
        elif klass==compiler.ast.CallFunc:
            return self.callingExpression(expr, dest)

        ## lambda:
        elif klass==compiler.ast.Lambda:
            return self.lambdaExpression(expr, dest)

        ## stuff to do... :)
        else:
            print
            print
            print "*** UNKNOWN EXPRESSION ****"
            print expr
            print "*** entering debugger ****"
            print
            print
            import pdb; pdb.set_trace()


    infixOps = {
        compiler.ast.Add: "+",
        compiler.ast.Sub: "-",
        compiler.ast.Mul: "*",
        compiler.ast.Div: "/",
        compiler.ast.Mod: "%",
        #compiler.ast.Power: "**",        # doesn't work yet
        #compiler.ast.RightShift: '>>',   # untested
        #compiler.ast.LeftShift: '<<',    # untested
    }

        
    def infixExpression(self, expr, dest):
        operator = self.infixOps[expr.__class__]
        symleft,  typleft  = self.symbol("$P"), "PerlInt"
        symright, typright = self.symbol("$P"), "PerlInt"
        symexpr,  typexpr  = self.symbol("$P"), "PerlInt"
        res = []

        # store left side of expression in symleft:
        res.append("%s = new %s" % (symleft, typleft))
        res.extend(self.expression(expr.left, symleft))

        # store right side of expression in symright:
        res.append("%s = new %s" % (symright, typright))
        res.extend(self.expression(expr.right, symright))

        # store the combined value in symexpr
        res.append("%s = new %s" % (symexpr, typexpr))
        res.append("%s = %s %s %s" \
                   % (symexpr, symleft, operator, symright))

        # and finally put the result in our destination
        # (imcc seems to like this as a separate step for
        # typecasting or something...)
        res.append("%s = %s" % (dest, symexpr))
        return res
    

    def compareExpression(self, expr, dest):
        assert len(expr.ops) == 1, "multi-compare not working yet"
        res = []       
        # get left side:
        symL = self.symbol("$P")
        res.extend(self.expression(expr.expr, symL))

        # get the op:
        op, code = expr.ops[0]
        if op=="<>": op="!="
        
        # get right side:
        symR = self.symbol("$P")
        res.extend(self.expression(code, symR))

        _cmp = self.symbol("_cmp")
        _end = self.symbol("_end")
        res.append("%s = new PerlInt" % dest)
        res.append("if %s %s %s goto %s" % (symL, op, symR, _cmp))
        res.append("%s = 0" % dest)
        res.append("goto %s" % _end)
        res.append("%s:" % _cmp)
        res.append("%s = 1" % dest)
        res.append("%s:" % _end)
        return res


    logicOps = {
        compiler.ast.Not: '!',
        compiler.ast.And: 'and',
        compiler.ast.Or: 'or',
    }


    def logicExpression(self, expr, dest):
        operator = self.logicOps[expr.__class__]
        res = []
        if operator == "!":            
            res.extend(self.expression(expr.expr, dest))
            res.append("not %s, %s" % (dest,dest))
        else:
            L,R = expr.nodes
            tmp = self.symbol("tmp")
            res.append(".local PerlInt %s" % tmp)
            res.extend(self.expression(L, dest))
            res.extend(self.expression(R, tmp))
            res.append("%s %s, %s, %s" % (operator, dest, dest, tmp))
        return res


    def callingExpression(self, node, dest):
        assert not (node.star_args or node.dstar_args), \
               "f(*x,**y) not working yet"
        
        #assert node.node.__class__ != compiler.ast.Lambda, \
        #       "can't call lambdas directly yet" # @TODO: fix this!
        res = []
        node.args.reverse()
        for arg in node.args:
            var = self.symbol("arg")
            res.append(".local object %s" % var)
            res.extend(self.expression(arg, var))
            res.append(".arg %s" % var)
            

        # figure out what we're calling
        adr = self.symbol("$I")
        if isinstance(node.node, compiler.ast.Lambda):
            # lambdas don't have names!
            self.extend(self.lambdaExpression(node.node, adr, allocate=0))
        else:
            sub = node.node.name
            if sub.startswith("__py__"):
                # parrot sub. @TODO: "from __parrot__ import __py__print"
                res.append("%s = addr %s" % (adr, node.node.name))
            else:
                # normal sub:
                res.append("%s = %s" % (adr, sub))

        # do the jump and return the result
        # @TODO: use parrot calling conventions (invoke)
        res.append('jsr %s' % adr)
        if dest:
            res.append(".result %s" % dest)
        return res



    def lambdaExpression(self, node, dest, allocate=1):
        assert not node.kwargs or node.varargs, "only simple args for now"
        self.extend(self.set_lineno(node))

        # anonymous function, so no name
        sub = self.symbol("_sub")
        adr = self.symbol("$I")

        # fork a new code generator to walk the function's tree:
        vis = compiler.visitor.ASTVisitor()
        pir = PirateVisitor(sub,
                            doc="lambda from line %s" % node.lineno,
                            args=node.argnames)
        vis.preorder(compiler.ast.Return(node.code), pir)
        self.subs.append(pir)

        # store the address in dest
        res = ["%s = addr %s" % (adr, sub)]
        if allocate:
            res.append("%s = new PerlInt" % dest)
        res.append("%s = %s" % (dest, adr))
        return res



    ##[ visitor methods ]##########################################
        
    def visitPrint(self, node):
        assert node.dest is None, "print >> not yet handled"
        for n in node.nodes:
            self.extend(self.set_lineno(n))
            dest = self.symbol("$P")
            self.extend(self.expression(n, dest))
            self.append('.arg %s' % dest)
            self.append('call __py__print')
            self.append('print " "')


    def visitPrintnl(self, node):
        self.visitPrint(node)
        if node.nodes:
            self.unappend() # remove final space
        self.append('print "\\n"')


    def visitIf(self, node):
        _endif = self.symbol("_endif")
        
        for test, body in node.tests:

            # if not true, goto _elif
            self.extend(self.set_lineno(test))
            _elif = self.symbol("_elif")
            testvar = self.symbol("test")

            self.append(".local object %s" % testvar)
            self.append("%s = new PerlInt" % testvar)
            self.extend(self.expression(test, testvar))
            self.append("unless %s goto %s" % (testvar, _elif))
            
            # do it and goto _endif
            self.visit(body)
            self.append("goto %s" % _endif)
            
            # _elif: (next test or pass through to else)
            self.append("%s:" % _elif)

        # else:
        if node.else_:
            self.extend(self.set_lineno(node.else_))
            self.visit(node.else_)
            
        # _endif:
        self.append("%s:" % _endif)


    def visitAssign(self, node):
        leftside = node.nodes[0]
        rightside = node.expr
        if isinstance(leftside, compiler.ast.AssTuple):
            leftside = leftside.nodes
            rightside = rightside.nodes
        else:
            leftside = [leftside]
            rightside = [rightside]
        for node, expr in zip(leftside, rightside):
            name = node.name
            self.append(".local object %s" % name)
            self.extend(self.expression(expr, name))


    def visitWhile(self, node):
        assert node.else_ is None, "while...else not supported"
        self.extend(self.set_lineno(node))
        _while = self.symbol("while")
        _endwhile = self.symbol("endwhile")
        self.loops.append((_while, _endwhile))
        self.append("%s:" % _while)
        testvar = self.symbol("$P")
        self.append("%s = new PerlInt" % testvar)
        self.extend(self.expression(node.test, testvar))
        self.append("unless %s goto %s" % (testvar, _endwhile))
        self.visit(node.body)
        self.append("goto %s" % _while)
        self.append("%s:" % _endwhile)
        self.loops.pop()


    def visitFor(self, node):
        assert node.else_ is None, "for...else not supported"
        assert not isinstance(node.assign, compiler.ast.AssTuple), \
               "for x,y not implemented yet"

        self.extend(self.set_lineno(node))
        self.append(".local object %s" % node.assign.name)
        _for = self.symbol("_for")
        _endfor = self.symbol("_endfor")
        loopidx = self.symbol("idx")
        forlist = self.symbol("list")
        listlen = self.symbol("$I")

        self.loops.append((_for, _endfor))

        # first get the list
        self.append(".local PerlArray %s" % forlist)
        self.extend(self.expression(node.list, forlist))

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
        self.extend(self.callingExpression(node, dest=None))

    def visitReturn(self, node):
        _res= self.symbol("res")
        self.append(".local object %s" % _res)
        self.extend(self.expression(node.value, _res))
        self.append(".return %s" % _res)
        self.append("restoreall")
        self.append("ret")
        
        
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
    print >> i, open("pirate.imc").read()
    i.close()    
    return o.read()

if __name__=="__main__":
    import sys
    if len(sys.argv) > 1:
        src = open(sys.argv[-1]).read()
        if "-d" in sys.argv:
            print compile(src)
        else:
            print invoke(src)
    else:
        print __doc__
        sys.exit()

