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
    a list containing one imc instruction per line.
    it automagically figures out which line in pirate.py
    generated the code and appends that information 
    as a comment.
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
    
    def __init__(self, name, counter=None, depth=None):
        self.name = name
        self._last_lineno = None
        self.lines = imclist()
        self.loops = []
        self.counter = counter or {}
        self.subs = []
        self.vars = {}
        self.depth = depth or 0 # lexical scope depth
        self.globals = {}

    def symbol(self, prefix):
        """
        Return a unique symbol for label names in generated code
        """
        self.counter.setdefault(prefix,-1)
        self.counter[prefix] += 1
        return "%s%i" % (prefix, self.counter[prefix])

    def getCode(self):
        res  = ".sub %s\n" % self.name
        res += "    new_pad %s\n" % self.depth
        res += "\n".join(self.lines) + "\n"
        res += ".end\n"
        res += ".include 'pirate.imc'\n\n"
        res += "\n\n".join([s.getCode() for s in self.subs]) + "\n"
        return res
    

    def set_lineno (self, node):
        if (node.lineno is not None and
            node.lineno != self._last_lineno):
            self._last_lineno = node.lineno
            self.append('setline %i' % node.lineno)


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
            return 
        
        handler = {
            # new stye: return their own dest
            ast.Const:    self.expressConstant,

            ast.ListComp: self.expressListComp,
            
            # old style (return nothing)
            ast.Name:     self.nameExpression,
            ast.List:     self.listExpression,
            ast.Tuple:    self.listExpression, # tuples ares lists for now
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


    def expressConstant(self, expr, dest):
        t = type(expr.value)
        assert t in self.constMap, "unsupported const type:%s" % t
        if dest:
            self.append("%s = new %s" % (dest, self.constMap[t]))
            self.append("%s = %s" % (dest, repr(expr.value)))
            return dest
        else:
            return expr.value


    def nameExpression(self, expr, dest):
        #if expr.name in self.vars:
        self.append("find_lex %s, '%s'" % (dest, expr.name))
        #else:
        #    self.append("%s = %s" % (dest, expr.name))


    def listExpression(self, expr, dest):
        self.append("%(dest)s = new PerlArray" % locals())
        sym = self.symbol("$P")
        for item in expr.nodes:
            self.compileExpression(item, sym)
            self.append("push %s, %s" % (dest, sym))
        
    typemap = {
	str: "S",
	int: "I",
	float: "N"
    }
    def binaryExpression(self, node, dest):
        #@TODO: this isn't used yet (because it doesn't work
        # for all nodes and requires some refactoring) It's
        # leo's code for type inference... Probably ought
        # to go into the generic code generator. 
        
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
	return dest




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

        
    def infixExpression(self, node, dest):
        operator = self.infixOps[node.__class__]
        symleft  = self.symbol("$P")
        symright = self.symbol("$P")
        symexpr  = self.symbol("$P")

        # store left side of expression in symleft:
        self.compileExpression(node.left, symleft)

        # store right side of expression in symright:
        self.compileExpression(node.right, symright)

        # store the combined value in symexpr
        self.append("%s = new PerlUndef" % symexpr) #, typexpr))
        self.append("%s = %s %s %s" \
                    % (symexpr, symleft, operator, symright))

        # and finally put the result in our destination
        # (imcc seems to like this as a separate step for
        # typecasting or something...)
        self.append("%s = %s" % (dest, symexpr))
    

    def compareExpression(self, expr, dest):
        assert len(expr.ops) == 1, "multi-compare not working yet"

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
        self.append("%s = new PerlInt" % dest)
        self.append("if %s %s %s goto %s" % (symL, op, symR, _cmp))
        self.append("%s = 0" % dest)
        self.append("goto %s" % _end)
        self.append("%s:" % _cmp)
        self.append("%s = 1" % dest)
        self.append("%s:" % _end)


    logicOps = {
        ast.Not: '!',
        ast.And: 'and',
        ast.Or: 'or',
    }


    def logicExpression(self, expr, dest):
        operator = self.logicOps[expr.__class__]

        if operator == "!":            
            self.compileExpression(expr.expr, dest)
            self.append("not %s, %s" % (dest,dest))
        else:
            L,R = expr.nodes
            tmp = self.symbol("tmp")
            self.append(".local PerlInt %s" % tmp)
            self.compileExpression(L, dest)
            self.compileExpression(R, tmp)
            self.append("%s %s, %s, %s" % (operator, dest, dest, tmp))


    def callingExpression(self, node, dest):
        assert not (node.star_args or node.dstar_args), \
               "f(*x,**y) not working yet"
        
        args = []
        node.args.reverse()
        for arg in node.args:
            var = self.symbol("arg")
            self.append(".local object %s" % var)
            self.compileExpression(arg, var)
            args.append(".arg %s" % var)

        # figure out what we're calling
        sub_pmc = self.symbol("$P")
        if isinstance(node.node, ast.Lambda):
            # lambdas don't have names!
            self.lambdaExpression(node.node, sub_pmc, allocate=0)
        else:
            sub = node.node.name
            self.append("find_lex %s, '%s'" % (sub_pmc, sub)) 

        ret = self.symbol("ret")
        ret_pmc = self.symbol("$P")

        ## now call it:
        self.append("newsub %s, .Continuation, %s" % (ret_pmc, ret))
        self.append(".pcc_begin non_prototyped")
        for r in args:
            self.append(r)
        self.append('.pcc_call %s, %s' % (sub_pmc, ret_pmc))
        self.append('%s:' % ret)
        if dest:
            self.append(".result %s" % dest)
        self.append(".pcc_end")


    def lambdaExpression(self, node, dest, allocate=1):
        return self.genFunction(node,dest,allocate)


    def genFunction(self, node, dest, allocate=1):
        assert not node.kwargs or node.varargs, "only simple args for now"
        self.set_lineno(node)

        # functions are always anonymous, so make fake names
        sub = self.symbol("_sub")
        ref = self.symbol("$P")

        # but sometimes they have a name bound to them (def vs lambda):
        isLambda = not hasattr(node, "name")
        
        if isLambda:
            comment = "lambda from line %s" % (node.lineno)
        else:
            comment = "%s from line %s" % (node.name, node.lineno)
            
        # fork a new code generator to walk the function's tree:
        vis = compiler.visitor.ASTVisitor()
        pir = PirateSubVisitor(sub,
                               depth = self.depth+1,
                               doc=comment,
                               counter = self.counter,
                               vars = self.vars,
                               args=node.argnames)

        # lambda is really just a single return function:
        if isLambda:            
            vis.preorder(ast.Return(node.code), pir)
        else:
            vis.preorder(node.code, pir)
            
        self.subs.append(pir)

        # store the address in dest
        self.append("newsub %s, .Closure, %s" % (ref, sub))
        if allocate:
            self.append("store_lex -1, '%s', %s" % (dest, ref))
            self.vars[dest] = dest
        else:
            self.append("%s = %s" % (dest, ref))


    ##[ list comprehensions ]#######################################

    # This section transforms a list comprhension into a
    # set of nested for and if blocks.
    #
    # It's in the middle here because it uses expressXXX() AND visitXXX()
    #
    # @TODO: put this tree transformation in its own Visitor class
    # but -- if we do that, we can only use nodes, not self.append()

    global ListCompExpr # yech, but I wanted this all in one place.
    class ListCompExpr(ast.Node):
        def __init__(self, expr, pmc):
            self.expr = expr
            self.pmc = pmc

    def expressListComp(self, node, dest):
        self.set_lineno(node)
        lcval = self.symbol("$P")
        self.append("%(dest)s = new PerlArray" % locals())
        queue = node.quals + [ListCompExpr(node.expr, dest)]
        self.visit(self.comprehend(queue))
        return dest

    def comprehend(self, queue):
        """
        do our own walk of the tree and rebuild
        using ast.For and ast.If
        """
        head, tail = queue[0], queue[1:]
        if isinstance(head, ast.ListCompFor):
            return ast.For(assign = head.assign,
                           list = head.list,
                           body = self.comprehend(head.ifs + tail),
                           else_ = None)                         
        elif isinstance(head, ast.ListCompIf):
            return ast.If(tests = [(head.test, self.comprehend(tail))],
                          else_ = None)
        elif isinstance(head, ListCompExpr):
            return head
        else:
            raise "i can't comprehend a %s" % head.__class__.__name__
        

    def visitListCompExpr(self, node):
        exp = self.symbol("$P")
        pmc = node.pmc
        self.compileExpression(node.expr, exp)
        self.append("push %(pmc)s, %(exp)s" % locals())
        

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
        if isinstance(leftside, ast.AssTuple) \
        or isinstance(leftside, ast.AssList):
            leftside = leftside.nodes
            rightside = rightside.nodes
        elif isinstance(leftside, ast.AssName):
            leftside = [leftside]
            rightside = [rightside]
        else:
            raise NotImplementedError, \
                  "don't how to handle %s" % node.__class__.__name__
            
        for node, expr in zip(leftside, rightside):
            name = node.name

            #@TODO: get rid of local object nonsense here...
            if isinstance(expr, ast.Lambda):
                self.compileExpression(expr, name)
            else:
                self.append(".local object %s" % name)
                self.compileExpression(expr, name)
                if name in self.globals:
                    self.append("store_lex  0, '%s', %s" % (name,name))
                else:
                    self.append("store_lex -1, '%s', %s" % (name,name))


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

        value = self.symbol("$P")
        name = node.assign.name
        self.append("%(value)s = %(forlist)s[%(loopidx)s]" % locals())
        self.append("store_lex -1, '%(name)s', %(value)s"  % locals())
        self.append("%(loopidx)s = %(loopidx)s + 1" % locals())
        
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
        self.append("pop_pad")
        self.append(".pcc_begin_return")
        self.append(".return %s" % _res)
        self.append(".pcc_end_return")

    def visitPass(self, node):
        self.append("noop")

    def visitFunction(self, node):  # visitDef
        #self.append(".local object %s" % node.name) #@TODO: use set_lex
        self.genFunction(node, node.name, allocate=1)
                


class PirateSubVisitor(PirateVisitor):
    """
    I am just like the normal visitor, but
    work on subroutines instead of whole
    programs.
    """
    def __init__(self, name, depth, doc, counter, vars, args=[]):
        super(PirateSubVisitor, self).__init__(name, counter=counter)
        self.doc = doc
        self.args = args
        self.vars = vars
        for arg in self.args:
            self.vars[arg]=arg
        self.counter = counter
        self.depth = depth
        self.globals = []
        
    def getCode(self):
        res = ""
        if self.doc:
            res += "# %s\n" % self.doc
        res  += ".pcc_sub %s non_prototyped\n" % self.name
        for arg in self.args:
            res += "    .param object %s\n" % arg
        res += "    new_pad %s\n" % self.depth
        for arg in self.args:
            res += "    store_lex -1, '%s', %s\n" % (arg,arg)
        res += "\n".join(self.lines) + "\n"
        res += "    .local object None\n"
        res += "    None = new PerlString\n"  #@TODO: .PythonNone
        res += "    None = 'None'\n"
        res += "    .pcc_begin_return\n"
        res += "    .return None\n"
        res += "    .pcc_end_return\n"
        res += ".end\n\n"
        res += "\n\n".join([s.getCode() for s in self.subs])
        return res
    
    def visitGlobal(self, node):
        for var in node.names:
            self.globals.append(var)


        
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
    i,o = os.popen4("parrot -")
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
        
