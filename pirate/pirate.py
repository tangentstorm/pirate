#!/usr/bin/python
"""
pirate: python->parrot compiler
 usage: pirate.py [-d] filename.py
        -d dumps the generated parrot code
"""

import os
import compiler
import traceback
from compiler import ast


def enumerate(seq):
    return [(i,seq[i]) for i in range(len(seq))]


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
        #@TODO: use inspect.stack() to get locals()?
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
        res += "    new_pad 0\n"
        res += "    newsub P0, .Exception_Handler, __py__catch\n"
        res += "    set_eh P0\n"
        res += "\n".join(self.lines) + "\n"
        res += ".end\n"
        res += ".include 'pirate.imc'\n\n"
        res += "\n\n".join([s.getCode() for s in self.subs]) + "\n"
        return res

    def imcObject(self, name, type=None):
        symbol = self.symbol(name)
        self.append(".local object %(symbol)s" % locals())
        self.append("%(symbol)s = new PerlUndef" % locals())
        return symbol

    
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
    
    def compileExpression(self, node, dest, allocate=1):
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
            ast.Const:     self.expressConstant,
            ast.Dict:      self.expressDict,
            ast.Subscript: self.expressSubscript,
            ast.ListComp:  self.expressListComp,

            ast.Getattr:   self.expressGetattr,
            
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

            ast.UnarySub:    self.expressUnary,
            ast.UnaryAdd:    self.expressUnary,
            ast.Invert:      self.expressUnary, 
            
            ast.Add: self.infixExpression,
            ast.Sub: self.infixExpression,
            ast.Mul: self.infixExpression,
            ast.Div: self.infixExpression,
            ast.Mod: self.infixExpression,
            ast.LeftShift: self.infixExpression,
            ast.RightShift: self.infixExpression,

            ast.Bitor: self.expressBitwise,
            ast.Bitxor: self.expressBitwise,            
            ast.Bitand: self.expressBitwise,

            # leo's patch:
            #ast.Sub: self.binaryExpression,
        }
        try:
            meth = handler[node.__class__]
        except KeyError:
            print "## unknown expression:"
            print node
            print "## entering debugger..."
            print
            import pdb; pdb.set_trace()

        # still here, so...
        return meth(node, dest, allocate)

    constMap = {
        str: "PerlString",
        int: "PerlInt",
    }


    def expressConstant(self, node, dest, allocate):
        t = type(node.value)
        assert t in self.constMap, "unsupported const type:%s" % t
        if dest:
            if allocate:
                self.append("%s = new %s" % (dest, self.constMap[t]))
            self.append("%s = %s" % (dest, repr(node.value)))
            return dest
        else:
            return node.value


    def expressDict(self, node, dest, allocate):
        self.set_lineno(node)
        self.append("%(dest)s = new PerlHash" % locals())
        if node.items:
            key = self.symbol("key")
            val = self.symbol("val")
            self.append(".local Key %(key)s" % locals())
            self.append(".local object %(val)s" % locals())
            for k,v in node.items:
                self.append("%(key)s = new Key"  % locals())
                self.compileExpression(k, key, allocate=0)
                self.compileExpression(v, val)
                self.append("%(dest)s[%(key)s] = %(val)s" % locals())
        return dest



    def assertNotUndefined(self, what, error):
        # @TODO: remove thess runtime checks once Pyobjects work
        # parrotclass.getprop returns PerlUndef if not present so:
        endCheck = self.symbol("endCheck")
        self.append("typeof $S0, %(what)s" % locals())
        #self.append("print $S0")
        self.append("unless $S0 == 'PerlUndef' goto %(endCheck)s" % locals())
        ex = ast.Raise(ast.Const(error), None, None)
        self.visit(ex)
        self.append("%(endCheck)s:" % locals())


    def expressSubscript(self, node, dest, allocate):
        assert len(node.subs) == 1, "huh? multiple subscripts?" # huh?
        
        dict = self.symbol("dict")
        self.append(".local object %(dict)s" % locals())
        self.compileExpression(node.expr, dict, allocate=0)
        
        subs = self.symbol("subs")
        self.append(".local object %(subs)s" % locals())
        self.append("%(subs)s = new Key" % locals())
        assert len(node.subs) == 1
        self.compileExpression(node.subs[0], subs, allocate=0)

        slot = "%(dict)s[%(subs)s]" % locals()

        if node.flags != "OP_ASSIGN":
            check = self.symbol("$P")
            self.append("%(check)s = %(slot)s" % locals())
            key = getattr(node.subs[0], "value", node.subs[0])
            self.assertNotUndefined(check, "KeyError: %(key)s" % locals())

        if node.flags == "OP_DELETE":
            self.append("delete %(slot)s" % locals())
        elif dest:
            self.append("%(dest)s = %(slot)s" % locals())
            return dest
        else:
            return slot


    def expressGetattr(self, node, dest, allocate):
        self.set_lineno(node)
        attr = node.attrname
        obj = self.imcObject("obj")
        self.compileExpression(node.expr, obj, allocate=1)
        self.append("getprop %(dest)s, '%(attr)s', %(obj)s" % locals())
        self.assertNotUndefined(dest, 'AttributeError: %(attr)s' % locals())
        return dest
    
        
    def nameExpression(self, node, dest, allocate):
        self.append("find_lex %s, '%s'" % (dest, node.name))


    def listExpression(self, expr, dest, allocate):
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
    def binaryExpression(self, node, dest, allocate):
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


    unaryOps = {
        ast.UnaryAdd: " ", # do absolutely nothing at all!!
        ast.UnarySub: "-",
        ast.Invert:   "~",
    }

    def expressUnary(self, node, dest, allocate):
        assert allocate==1
        value = self.imcObject("value")
        self.compileExpression(node.expr, value)
        op = self.unaryOps[node.__class__]
        self.append("%(dest)s = new PerlUndef" % locals())
        self.append("%(dest)s = %(op)s%(value)s" % locals())
        return dest


    bitwiseOps = {
        ast.Bitand: "&",
        ast.Bitor: "|",
        ast.Bitxor: "~", # weird but true
    }
    
    def expressBitwise(self, node, dest, allocate):
        assert allocate == 1
        value = self.imcObject("value")
        next = self.imcObject("next")
        
        op = self.bitwiseOps[node.__class__]
        self.compileExpression(node.nodes[0], value)
        for n in node.nodes[1:]:
            self.compileExpression(n, next)
            self.append("%(value)s = %(value)s %(op)s %(next)s" % locals())

        
        
        #Bitand: "&",
        self.append("%(dest)s = %(value)s" % locals())
        return dest

    infixOps = {
        ast.Add: "+",
        ast.Sub: "-",
        ast.Mul: "*",
        ast.Div: "/",
        ast.Mod: "%",
        ast.RightShift: '>>',   # untested
        ast.LeftShift: '<<',    # untested
    }
        
    def infixExpression(self, node, dest, allocate):
        lside = self.imcObject("lside")
        rside = self.imcObject("rside")
        self.compileExpression(node.left, lside)
        self.compileExpression(node.right, rside)

        op = self.infixOps[node.__class__]
        result = self.imcObject("result")
        self.append("%(result)s = %(lside)s %(op)s %(rside)s" % locals())

        # put the result in our destination
        # (imcc seems to like this as a separate step
        # for typecasting or something...)
        self.append("%(dest)s = %(result)s" % locals())
    

    def compareExpression(self, expr, dest, allocate):
        assert len(expr.ops) == 1, "@TODO: multi-compare not working yet"

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


    def logicExpression(self, expr, dest, allocate):
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


    def callingExpression(self, node, dest, allocate):
        assert not (node.star_args or node.dstar_args), \
               "@TODO: f(*x,**y) not working yet"
        
        args = []
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
        elif isinstance(node.node, ast.Getattr):
            self.compileExpression(node.node, sub_pmc)
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


    def lambdaExpression(self, node, dest, allocate=0):
        return self.genFunction(node,dest,allocate=0)


    def genFunction(self, node, dest, allocate=1):
        assert not node.kwargs or node.varargs, \
               "@TODO: only simple args for now"
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
        elif dest:
            #pass
            self.append("%s = %s" % (dest, ref))


    ##[ list comprehensions ]#######################################

    ## This section transforms a list comprhension into a
    ## set of nested for and if blocks.
    ##
    ## It's in the middle here because it uses expressXXX() AND visitXXX()
    ##
    ## @TODO: put this tree transformation in its own Visitor class
    ## but -- if we do that, we can only use nodes, not self.append()

    global ListCompExpr # yech, but I wanted this all in one place.
    class ListCompExpr(ast.Node):
        def __init__(self, expr, pmc):
            self.expr = expr
            self.pmc = pmc

    def expressListComp(self, node, dest, allocate):
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
        assert node.dest is None, "@TODO: print >> not yet handled"
        for n in node.nodes:
            self.set_lineno(n)
            dest = self.symbol("$P")
            self.compileExpression(n, dest)
            self.append('.arg 0') # not nested
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

            testvar = self.imcObject("test")


            self.compileExpression(test, testvar)
            self.append("unless %(testvar)s goto %(_elif)s" % locals())
            
            # do it and goto _endif
            self.visit(body)
            self.append("goto %(_endif)s" % locals())
            
            # _elif: (next test or pass through to else)
            self.append("%(_elif)s:" % locals())

        # else:
        if node.else_:
            self.set_lineno(node.else_)
            self.visit(node.else_)
            
        # _endif:
        self.append("%(_endif)s:" % locals())


    ##[ assignment ]################################################

    def assign(self, node, value):
        """
        dispatch method for name/property/subscript
        """
        method = {
            ast.Subscript: self.assignToSubscript,
            ast.AssAttr: self.assignToProperty,
            ast.AssName: self.assignToName,
            }[node.__class__]
        method(node, value)

    def assignToSubscript(self, node, value):
        slot = self.expressSubscript(node, dest=None, allocate=0)
        self.append("%(slot)s = %(value)s" % locals())


    def assignToProperty(self, node, value):
        obj = self.symbol("obj")
        self.append(".local object %(obj)s" % locals())
        self.compileExpression(node.expr, obj)
        attr = node.attrname
        self.append("setprop %(obj)s, '%(attr)s', %(value)s" %locals())


    def assignToName(self, node, value):
        name = node.name
        if name in self.globals:
            self.append("store_lex  0, '%(name)s', %(value)s" % locals())
        else:
            self.append("store_lex -1, '%(name)s', %(value)s" % locals())


    def evaluate(self, expr):
        """
        stores an expression in a P-register and
        return a symbol for that register.
        """
        value = self.symbol("value")
        self.append(".local object %(value)s" % locals())
        self.compileExpression(expr, value)
        return value


    def visitAssign(self, node):

        def listify(side):
            # the children if present, or just the node in a list
            return getattr(side,"nodes",[side])

        lside = listify(node.nodes[0])
        rside = listify(node.expr)

        
        if len(rside)==0 or len(lside)==1:
            # eg, x = []
            self.assign(lside[0], self.evaluate(node.expr))

        elif (len(lside) == len(rside)):
            ## this works for l=r AND l=r,r,r,r
            ## because of AssTuple nodes :)
            for node, expr in zip(lside, rside):
                self.assign(node, self.evaluate(expr))

        elif len(rside)==1:
            ## this handles l,l,l=r and l,l,l=r()
            rside = rside[0]
            if isinstance(rside, ast.Const):
                ## @TODO: non-sequence check should be at *runtime*
                raise TypeError("unpack non-sequence")
            else:
                value = self.evaluate(rside)
                extract = self.symbol("extract")
                self.append(".local object %(extract)s" % locals())
                for (i, node) in enumerate(lside):
                    self.append("%(extract)s = %(value)s[%(i)s]" % locals())
                    self.assign(node, extract)
        else:
            ## i don't THINK there are any other combinations... (??)
            ## @TODO: unpack wrong size check should also be at runtime
            raise ValueError("unpack sequence of wrong size")

    ##[ del ]######################################################

    def visitAssName(self, node):
        """
        As far as I can tell, this node is only used for 'del name'
        """
        assert node.flags == "OP_DELETE", "expected AssName to be a del!"
        pad = self.symbol("pad")
        name = node.name
        self.append(".local object %(pad)s" % locals())
        self.append("peek_pad %(pad)s" % locals())
        self.append("delete %(pad)s['%(name)s']" % locals())

    def visitAssAttr(self, node):
        assert node.flags == "OP_DELETE", "expected AssGetattr to be a del!"
        assert isinstance(node.expr, ast.Name), "only del name.attr allowed"
        # @TODO: allow del (expression).name
        # can't do this yet because of store_lex
        var = node.expr.name
        atr = node.attrname
        obj = self.imcObject("obj")
        self.compileExpression(node.expr, obj)
        self.append("delprop %(obj)s, '%(atr)s'" % locals())
        
        # this next line is really really wrong:
        #  - it really should be modifying the actual object, not a copy.
        #  - multiple find_lex and store_lex should be optimized out anyway.
        self.append("store_lex '%(var)s', %(obj)s" % locals())


    def visitSubscript(self, node):
        assert node.flags == "OP_DELETE"
        self.expressSubscript(node, dest=None, allocate=0)


    ##[ control stuctures ]#########################################

    def visitWhile(self, node):
        assert node.else_ is None, "@TODO: while...else not supported"
        self.set_lineno(node)
        _while = self.symbol("while")
        _endwhile = self.symbol("endwhile")
        self.loops.append((_while, _endwhile))
        
        testvar = self.symbol("$P")
        self.append("%(_while)s:" % locals())
        self.append("%(testvar)s = new PerlInt" % locals())
        self.compileExpression(node.test, testvar)
        self.append("unless %(testvar)s goto %(_endwhile)s" % locals())
        self.visit(node.body)
        self.append("goto %(_while)s" % locals())
        self.append("%(_endwhile)s:" % locals())
        self.loops.pop()


    def visitFor(self, node):
        assert node.else_ is None, "@TODO: for...else not supported"
        assert not isinstance(node.assign, ast.AssTuple), \
               "@TODO: for x,y not implemented yet"

        name = node.assign.name
        namesym = self.symbol(node.assign.name)

        self.set_lineno(node)
        self.append(".local object %s" % namesym)
        _for = self.symbol("for")
        _endfor = self.symbol("endfor")
        self.loops.append((_for, _endfor))

        loopidx = self.symbol("idx")
        forlist = self.symbol("list")
        listlen = self.symbol("$I")

        # first get the list
        self.append(".local PerlArray %s" % forlist)
        self.compileExpression(node.list, forlist)

        self.append("%(listlen)s = %(forlist)s" % locals())
        self.append(".local int %(loopidx)s" % locals())
        self.append("%(loopidx)s = 0" % locals())

        # get the next item (also where "continue" jumps to)
        self.append("%(_for)s:" % locals())
        self.append("save %(forlist)s" % locals())
        self.append("%s = %s[%s]" % (namesym, forlist, loopidx))

        value = self.symbol("$P")
        self.append("%(value)s = %(forlist)s[%(loopidx)s]" % locals())
        self.append("store_lex -1, '%(name)s', %(value)s"  % locals())
        self.append("%(loopidx)s = %(loopidx)s + 1" % locals())
        
        # do the loop body
        self.visit(node.body)

        self.append("restore %(forlist)s" % locals())

        # now loop!
        self.append("if %(loopidx)s < %(listlen)s goto %(_for)s" % locals())
        self.append("%(_endfor)s:" % locals())
        
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
        self.callingExpression(node, dest=None, allocate=0)

    def visitDiscard(self, node):
        #import pdb; pdb.set_trace()
        # PARROT_INLINE is a special macro for inling parrot code
        try:
            nevermind = 1
            name = node.expr.node.name
            if name == "PARROT_INLINE":
                nevermind = 0
        except:
            pass

        if nevermind:
            self.visit(node.expr)
        else:
            for line in node.expr.args:
                assert isinstance(line, ast.Const), "can only INLINE strings"
                self.append(line.value)
            

    def visitPass(self, node):
        self.append("noop")

    def visitFunction(self, node):  # visitDef
        self.genFunction(node, node.name, allocate=1)

    def visitReturn(self, node):
        raise SyntaxError, "return outside of function"

    def visitYield(self, node):
        raise SyntaxError, "yield outside of function"
        
                
    ##[ exceptions ]####################################################

    def visitRaise(self, node):
        assert node.expr1, "argument required for raise"
        assert not (node.expr2 or node.expr3), "only 1 arg alllowed for raise"
        self.set_lineno(node)
        ex = self.symbol("ex")
        msg = self.symbol("msg")
        self.append(".local object %(ex)s " % locals())
        self.append(".local object %(msg)s " % locals())        
        self.append("%(ex)s = new Exception " % locals())
        self.compileExpression(node.expr1, msg)
        sreg = self.symbol("$S")
        self.append("%(sreg)s = %(msg)s" % locals())
        self.append("%(ex)s['_message'] = %(sreg)s" % locals())
        self.append("throw %(ex)s" % locals())

    def visitAssert(self, node):
        # another tree transformation -- if not .test: raise .fail
        self.visit( ast.If(
            tests = [(ast.Not(node.test),
                      ast.Raise(node.fail, None, None))],
            else_ = None ))


    def visitTryExcept(self, node):
        self.set_lineno(node)
        assert len(node.handlers)==1, "@TODO: only one handler for now"
        assert not node.else_, "@TODO: try...else not implemented"
        catch = self.symbol("catch")
        handler = self.symbol("handler")
        endtry = self.symbol("endtry")
        self.append(".local Sub %(handler)s" % locals())        
        self.append("newsub %(handler)s, .Exception_Handler, %(catch)s" \
                    % locals())
        self.append("set_eh %(handler)s" % locals())
        self.visit(node.body)
        self.append("clear_eh")
        self.append("goto %(endtry)s" % locals())
        self.append("%(catch)s:" % locals())
        for hand in node.handlers:
            expr, target, body = hand
            assert not (expr or target), \
                   "@TODO: can't get exception object yet"
            self.visit(body)
        self.append("%(endtry)s:" % locals())

                
        
    def visitTryFinally(self, node):
        self.set_lineno(node)
        final = self.symbol("final")
        handler = self.symbol("final")
        self.append(".local Sub %(handler)s" % locals())
        self.append("newsub %(handler)s, .Exception_Handler, %(final)s" \
                    % locals())
        self.append("set_eh %(handler)s" % locals())
        self.visit(node.body)
        self.append("clear_eh")
        self.append("%(final)s:" % locals())
        self.visit(node.final) 


    def visitClass(self, node):
        name = node.name
        klass = self.symbol("klass")
        cname = self.symbol("cname")
        self.append(".local object %(klass)s" % locals())
        self.append(".local object %(cname)s" % locals())
        self.append("newclass %(klass)s, '%(name)s'" % locals())
        self.append("%(cname)s = new PerlString" % locals())
        self.append("%(cname)s = '%(name)s'" % locals())
        self.append("setprop %(klass)s, '__name__', %(cname)s" % locals())
        self.append("store_lex -1, '%(name)s', %(klass)s" % locals())

        

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
        self.isGenerator = 0
        
    def getCode(self):
        if self.isGenerator:
            res = self.getCodeForGenerator()
        else:
            res = self.getCodeForFunction()
        return res + "\n\n".join([s.getCode() for s in self.subs])

    def getCodeForFunction(self):
        res = []
        if self.doc:
            res.append("# %s" % self.doc)
        res.append(".pcc_sub %s non_prototyped" % self.name)
        for arg in self.args:
            res.append("    .param object %(arg)s" % locals())
        res.append("    new_pad %s" % self.depth) #@TODO: use -1 here??
        for arg in self.args:
            res.append("    store_lex -1, '%(arg)s', %(arg)s" % locals())
        res.extend(self.lines)
        res.append("    #------")
        res.append("    .local object None")
        res.append("    None = new PerlString")  #@TODO: .PythonNone
        res.append("    None = 'None'")
        res.append("    .pcc_begin_return")
        res.append("    .return None")
        res.append("    .pcc_end_return")
        res.append(".end")
        res.append("")
        return "\n".join(res)

    def getCodeForGenerator(self):
        
        stop = ast.Raise(ast.Const("StopIteration"), None, None)
        self.visit(stop)

        name = self.name
        res = []
        res.append(".pcc_sub %(name)s non_prototyped" % locals())
        res.append("   .local object gen_fun")
        res.append("   .local object gen_obj")
        res.append("   newsub gen_fun, .Coroutine, %(name)s_g" % locals())
        res.append("   gen_obj = new ParrotObject")
        res.append("   setprop gen_obj, 'next', gen_fun")
        res.append("   .pcc_begin_return")
        res.append("   .return gen_obj")
        res.append("   .pcc_end_return")
        res.append(".end")
        res.append(".pcc_sub %(name)s_g non_prototyped" % locals())
        res.append("   new_pad -1")
        res.extend(self.lines)
        res.append(".end")
        return "\n".join(res)
    
    def visitGlobal(self, node):
        for var in node.names:
            self.globals.append(var)

    def visitReturn(self, node):
        #@TODO: allow both yield and return in one function
        result = self.imcObject("res")
        self.compileExpression(node.value, result)
        self.append("pop_pad")
        self.append(".pcc_begin_return")
        self.append(".return %(result)s" % locals())
        self.append(".pcc_end_return")

    def visitYield(self, node):
        # @TODO: any way to consolidate this with return?
        self.isGenerator = 1
        res = self.imcObject("res")
        self.compileExpression(node.value, res)
        #self.append("saveall")
        #self.append("pop_pad")
        self.append(".pcc_begin_yield")
        self.append(".return %(res)s" % locals())
        self.append(".pcc_end_yield")
        #self.append("restoreall")
        
        
                
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
        
