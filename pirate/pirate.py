#!/usr/bin/python
"""
pirate: python->parrot compiler
 usage: pirate.py [-d] filename.py
        -d dumps the generated parrot code
"""

# TODO: change imclist to be aware of var/const differences, take
# instructions and values -- eventually be essentially an AST

# dump use of scratchpads entirely, work out proper scheme for nested
# funs, use a dict for globals, put locals in named temps, un-name
# real temps

# do type inference to unbox locals.


import os
import compiler
import traceback
from compiler import ast

if not hasattr(__builtins__,'enumerate'):
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
        is_setline = line.startswith("setline")
        if ':' not in line:
            line = "    %-30s"  %line
            if "#" not in line:
                if not is_setline: # work round IMCC parsing bug
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
        self.classKnown = [] # only make one constructor per class
        self.classStack = [] # the class we're looking at right now

    def gensym(self, prefix="$P", type="object"):
        """
        Return a unique symbol for variable names in generated code.
        """
        self.counter.setdefault(prefix,-1)
        self.counter[prefix] += 1
        sym = "%s%i" % (prefix, self.counter[prefix]) 
        if type:  
            self.append(".local %s %s" % (type, sym))
        return sym

    def genlabel(self, prefix):
        """
        Return a unique symbol for label names in generated code.
        """
        self.counter.setdefault(prefix,-1)
        self.counter[prefix] += 1
        sym = "%s%i" % (prefix, self.counter[prefix]) 
        return sym

    def getCode(self):
        res  = ".sub %s\n" % self.name
        res += "    new_pad 0\n"
        res += "    newsub P0, .Exception_Handler, __py__catch\n"
        res += "    set_eh P0\n"
        res += "    newclass P0, \"PythonIterator\" \n"
        res += "\n".join(self.lines) + "\n"
        res += ".end\n"
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
        
    def bindLocal(self, name, value):
        return "store_lex -1, '%s', %s" % (name, value)
    def bindGlobal(self, name, value):
        return "store_lex  0, '%s', %s" % (name, value)

    def lookupName(self, name):
        dest = self.gensym('foo')
        self.append("find_lex %s, '%s'" % (dest, name))
        return dest
    ##[ expression compiler ]######################################
    
    def compileExpression(self, node, allocate=0):
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
            ast.Tuple:    self.tupleExpression,
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
        return meth(node, allocate)

    constMap = {
        str: "PerlString",
        int: "PerlInt",
        float: "Float"
    }


    def expressConstant(self, node, allocate):
        t = type(node.value)
        assert t in self.constMap, "unsupported const type:%s" % t
        if allocate:
            dest = self.gensym("const")
            self.append("%s = new %s" % (dest, self.constMap[t]))
            self.append("%s = %s" % (dest, repr(node.value)))
            return dest
        else:
            return repr(node.value)


    def expressDict(self, node, allocate):
        self.set_lineno(node)
        dest = self.gensym("dict")
        self.append("%s = new PerlHash" % dest)
        if node.items:
            key = self.gensym("key", "Key")
            for k,v in node.items:
                tmp = self.compileExpression(k)
                self.append("%s = new Key"  % key)
                self.append("%s = %s" % (key, tmp))
                val = self.compileExpression(v, allocate=1)
                self.append("%s[%s] = %s" % (dest, key, val))
        return dest



    def assertNotUndefined(self, what, error):
        # @TODO: remove thess runtime checks once Pyobjects work
        # parrotclass.getprop returns PerlUndef if not present so:
        endCheck = self.genlabel("endCheck")
        self.append("typeof $S0, %s" % what)
        #self.append("print $S0")
        self.append("unless $S0 == 'PerlUndef' goto %s" % endCheck)
        ex = ast.Raise(ast.Const(error), None, None)
        self.visit(ex)
        self.append("%s:" % endCheck)


    def expressSubscript(self, node, allocate):
        assert len(node.subs) == 1, "huh? multiple subscripts?" # huh?
        
        d = self.compileExpression(node.expr, allocate=0)
        subs = self.gensym('subs')
        self.append(subs + " = new Key")
        assert len(node.subs) == 1
        realsubs = self.compileExpression(node.subs[0], allocate=0)
        self.append("%s = %s" % (subs, realsubs))
        
        slot = "%s[%s]" % (d, subs)

        if node.flags != "OP_ASSIGN":
            check = self.gensym("ck")
            self.append("%s = %s" % (check, slot))
            key = getattr(node.subs[0], "value", node.subs[0])
            self.assertNotUndefined(check, "KeyError: " + str(key))

        if node.flags == "OP_DELETE":
            self.append("delete " + slot)
        elif node.flags=="OP_ASSIGN":
            return slot
        else:
            # imcc doesn't grok d[a][b] so set temp=d[a] and do temp[b]
            temp = self.gensym("temp")
            self.append("%s=%s" % (temp, slot))
            return temp


    def expressGetattr(self, node, allocate):
        dest = self.gensym("getattr")
        attr = node.attrname

        self.set_lineno(node)
        obj = self.compileExpression(node.expr, allocate=1)
        self.append("%s = __py__getattr(%s, '%s')" % (dest, obj, attr))

        # save base object in P% for calling conventions
        # @TODO: only do this if we're going to call the attr
        self.append("P5 = %s" % obj)
        return dest
    
        
    def nameExpression(self, node, allocate):
        dest = self.lookupName(node.name)
        return dest

    def listExpression(self, expr, allocate):
        dest = self.gensym("list", "PerlArray")
        self.append(dest + " = new PerlArray")
        for item in expr.nodes:
            sym = self.compileExpression(item)
            self.append("push %s, %s" % (dest, sym))
        return dest

    def tupleExpression(self, expr, allocate):
        dest = self.gensym("list", "FixedPMCArray")
        self.append(dest + " = new FixedPMCArray")
        self.append("%s=%d" % (dest,len(expr.nodes)))
        for i in range(0,len(expr.nodes)):
            sym = self.compileExpression(expr.nodes[i])
            self.append("%s[%d]=%s" % (dest, i, sym))
        return dest
    
    typemap = {
	str: "S",
	int: "I",
	float: "N"
    }
    
    unaryOps = {
        ast.UnaryAdd: " ", # do absolutely nothing at all!!
        ast.UnarySub: "-",
        ast.Invert:   "~",
    }

    def expressUnary(self, node, allocate):
        dest = self.gensym("unary")
        value = self.compileExpression(node.expr)
        op = self.unaryOps[node.__class__]
        self.append(dest + " = new PerlUndef")
        self.append("%s = %s%s" % (dest, op, value))
        return dest


    bitwiseOps = {
        ast.Bitand: "&",
        ast.Bitor: "|",
        ast.Bitxor: "~", # weird but true
    }
    
    def expressBitwise(self, node, allocate):
        dest = self.gensym("bitwise")        
        op = self.bitwiseOps[node.__class__]
        value = self.compileExpression(node.nodes[0], allocate=1)
        for n in node.nodes[1:]:
            next = self.compileExpression(n)
            self.append("%s = %s %s %s" % (value, value, op, next))
               
        #Bitand: "&",
        self.append("%s = %s" % (dest, value))
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
        
    def infixExpression(self, node, allocate):
        dest = self.gensym("infix")
        self.append("%s = new PerlUndef" % dest)
        lside = self.compileExpression(node.left, allocate=1)
        rside = self.compileExpression(node.right)
        if rside.startswith("'"):
            rside = self.compileExpression(node.right, allocate=1)
        
        op = self.infixOps[node.__class__]
        self.append("%s = %s %s %s" % (dest, lside, op, rside))

        # put the result in our destination
        # (imcc seems to like this as a separate step
        # for typecasting or something...)
        #wtf?!
        #self.append("%s = %s" % (dest, result))
        return dest

    def compareExpression(self, expr, allocate):
        assert len(expr.ops) == 1, "@TODO: multi-compare not working yet"
        dest = self.gensym("compare", "Boolean")
        # get left side:
        symL = self.compileExpression(expr.expr, allocate=1)

        # get the op:
        op, code = expr.ops[0]
        if op=="<>": op="!="
        
        # get right side:
        symR = self.compileExpression(code, allocate=1)

        _cmp = self.genlabel("cmp")
        _end = self.genlabel("endcmp")
        self.append("%s = new Boolean" % dest)
        self.append("if %s %s %s goto %s" % (symL, op, symR, _cmp))
        self.append("%s = 0" % dest)
        self.append("goto %s" % _end)
        self.append("%s:" % _cmp)
        self.append("%s = 1" % dest)
        self.append("%s:" % _end)
        return dest

    logicOps = {
        ast.Not: '!',
        ast.And: 'and',
        ast.Or: 'or',
    }


    def logicExpression(self, expr, allocate):
        operator = self.logicOps[expr.__class__]
        if operator == "!":            
            dest = self.genlabel("tmp")
            tmp = self.compileExpression(expr.expr, allocate=1)
            self.append(".local Boolean %s" % dest)
            self.append("%s = new Boolean" % dest)
            self.append("not %s, %s" % (dest,tmp))
        else:
            L,R = expr.nodes
            tmp = self.genlabel("tmp")
            self.append(".local Boolean %s" % tmp)
            self.append("%s = new Boolean" % tmp)
            dest = self.compileExpression(L, allocate=1)
            tmp = self.compileExpression(R, allocate=1) # until we come up with an unboxing scheme
            self.append("%s %s, %s, %s" % (operator, dest, dest, tmp))
        return dest

    def callingExpression(self, node, allocate):
        assert not (node.star_args or node.dstar_args), \
               "@TODO: f(*x,**y) not working yet"
        
        args = []
        for arg in node.args:
            var = self.compileExpression(arg, allocate=1)
            #self.append(".local object %s" % var)
            args.append(".arg %s" % var)

        # figure out what we're calling
        if isinstance(node.node, ast.Lambda):
            # lambdas don't have names!
            sub_pmc = self.lambdaExpression(node.node, allocate=0)
        elif isinstance(node.node, ast.Getattr):
            sub_pmc = self.compileExpression(node.node)
        else:
            sub_pmc = self.lookupName(node.node.name)

        ## now call it:
        self.append(".pcc_begin non_prototyped")
        for r in args:
            self.append(r)
        self.append('.pcc_call %s' % sub_pmc)
        dest = self.gensym("result")
        self.append(".result %s" % dest)
        self.append(".pcc_end")
        return dest


    def lambdaExpression(self, node, allocate=0):
        return self.genFunction(node, None, allocate=0)


    def genFunction(self, node,  name, allocate=1):
        assert not node.kwargs and not node.varargs, \
               "@TODO: only simple args for now"
        self.set_lineno(node)

        # functions are always anonymous, so make fake names
        sub = self.genlabel(self.name + "_" + str(name))
        ref = self.gensym("func")

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
            self.append(self.bindLocal(node.name, ref))
            self.vars[ref] = ref
        return ref

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

    def expressListComp(self, node, allocate):
        self.set_lineno(node)
        dest = self.gensym("listcomp")
        lcval = self.gensym("lcval")
        self.append(dest + " = new PerlArray")
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
        pmc = node.pmc
        exp = self.compileExpression(node.expr)
        self.append("push %s, %s" % (pmc, exp))
        

    ##[ visitor methods ]##########################################

    def visitPrint(self, node):
        assert node.dest is None, "@TODO: print >> not yet handled"
        for n in node.nodes:
            arg = self.compileExpression(n, allocate=1)
            self.append("__py__print(%s,0)" % arg)
            self.append("print ' '")
            

    def visitPrintnl(self, node):
        self.visitPrint(node)
        if node.nodes:
            self.unappend() # remove final space
        self.append('print "\\n"')


    def visitIf(self, node):
        _endif = self.genlabel("endif")
        
        for test, body in node.tests:

            # if not true, goto _elif
            self.set_lineno(test)
            _elif = self.genlabel("elif")

            testvar = self.compileExpression(test)
            self.append("unless %s goto %s" % (testvar, _elif))
            
            # do it and goto _endif
            self.visit(body)
            self.append("goto " + _endif)
            
            # _elif: (next test or pass through to else)
            self.append(_elif + ":")

        # else:
        if node.else_:
            self.set_lineno(node.else_)
            self.visit(node.else_)
            
        # _endif:
        self.append(_endif + ":")


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
        return method(node, value)

    def assignToSubscript(self, node, value):
        slot = self.expressSubscript(node, allocate=0)
        self.append("%s = %s" % (slot, value))


    def assignToProperty(self, node, value):
        obj = self.compileExpression(node.expr)
        attr = node.attrname
        self.append("setprop %s, '%s', %s" % (obj, attr, value))


    def assignToName(self, node, value):
        name = node.name
        if name in self.globals:
            self.append(self.bindGlobal(name, value))
        else:            
            self.append(self.bindLocal(name, value))
            # @TODO: bindAttribute 
            if self.classStack:
                self.append("setprop %s, '%s', %s"
                            % (self.classStack[-1], name, value))


    def visitAssign(self, node):

        def listify(side):
            # the children if present, or just the node in a list
            return getattr(side,"nodes",[side])

        lside = listify(node.nodes[0])
        rside = listify(node.expr)

        
        if len(rside)==0 or len(lside)==1:
            # eg, x = []
            self.assign(lside[0],
                        self.compileExpression(node.expr, allocate=1))

        elif (len(lside) == len(rside)):
            ## this works for l=r AND l=r,r,r,r
            ## because of AssTuple nodes :)
            for node, expr in zip(lside, rside):
                self.assign(node, self.compileExpression(expr, allocate=1))

        elif len(rside)==1:
            ## this handles l,l,l=r and l,l,l=r()
            rside = rside[0]
            if isinstance(rside, ast.Const):
                ## @TODO: non-sequence check should be at *runtime*
                raise TypeError("unpack non-sequence")
            else:
                value = self.compileExpression(rside)
                extract = self.gensym("extract")
                for (i, node) in enumerate(lside):
                    self.append("%s = %s[%s]" % (extract, value, i))
                    self.assign(node, extract)
        else:
            ## i don't THINK there are any other combinations... (??)
            ## @TODO: unpack wrong size check should also be at runtime
            raise ValueError("unpack sequence of wrong size")

    ##[ augmented assign ]#########################################

    def visitAugAssign(self, aug):
        """tranforms tree to a standard assignment and calls visitAssign
        
        @TODO: when Python Objects are available, this should try calling
        __iadd__ etc first.  It should only do a standard assignment if the
        object has no __iXXX__ method.
        """
        if isinstance(aug.node, ast.Name):  # e.g. x += 2
            lhs = ast.AssName(aug.node.name, 'OP_ASSIGN')
        elif isinstance(aug.node, ast.Getattr):   # e.g. (x+a).p += 2
            lhs = ast.AssAttr(aug.node.expr, aug.node.attrname, 'OP_ASSIGN')
        elif isinstance(aug.node, ast.Subscript):   # e.g. (x+a)[0] += 2
            lhs = ast.Subscript(aug.node.expr, 'OP_ASSIGN', aug.node.subs)

        ops = {'**=':'Power', '*=':'Mul', '/=':'Div', '%=':'Mod', '+=':'Add',
               '-=':'Sub', '<<=':'LeftShift', '>>=':'RightShift',
               '|=':'Bitor', '^=':'Bitxor', '&=':'Bitand'}
        op_node_class = getattr(ast, ops[aug.op])
        rhs = op_node_class((aug.node, aug.expr))
        new_node = ast.Assign([lhs], rhs)

        self.visitAssign(new_node)

    ##[ del ]######################################################

    def visitAssName(self, node):
        """
        As far as I can tell, this node is only used for 'del name'
        """
        assert node.flags == "OP_DELETE", "expected AssName to be a del!"
        pad = self.gensym("pad")
        name = node.name
        self.append("peek_pad " + pad)
        self.append("delete %s['%s']" % (pad, name))

    def visitAssAttr(self, node):
        assert node.flags == "OP_DELETE", "expected AssGetattr to be a del!"
        assert isinstance(node.expr, ast.Name), "only del name.attr allowed"
        # @TODO: allow del (expression).name
        # can't do this yet because of store_lex
        var = node.expr.name
        attr = node.attrname
        obj = self.compileExpression(node.expr)
        self.append("delprop %s, '%s'" % (obj, attr))
        
        # this next line is really really wrong:
        #  - it really should be modifying the actual object, not a copy.
        self.append(self.bindLocal(var, obj))

    def visitSubscript(self, node):
        assert node.flags == "OP_DELETE"
        self.expressSubscript(node, allocate=0)


    ##[ control stuctures ]#########################################

    def visitWhile(self, node):        
        self.set_lineno(node)
        _while = self.genlabel("while")
        _elsewhile = self.genlabel("elsewhile") # sync nums even if no "else"
        _endwhile = self.genlabel("endwhile")
        self.loops.append((_while, _endwhile))
        
        self.append(_while+ ":")
        testvar = self.compileExpression(node.test)
        self.append("unless %s goto %s" % (testvar, _elsewhile))
        self.visit(node.body)
        self.append("goto " + _while)
        self.append(_elsewhile + ":")
        if node.else_:
            self.visit(node.else_)
        self.append(_endwhile + ":")
        self.loops.pop()


    def visitFor(self, node):
        assert not isinstance(node.assign, ast.AssTuple), \
               "@TODO: for x,y not implemented yet"
        self.set_lineno(node)
        name = self.gensym("for_%s_" % node.assign.name)
        _for = self.genlabel("for")
        _endfor = self.genlabel("endfor")
        _elsefor = self.genlabel("elsefor")
        self.loops.append((_for, _endfor))

        loopidx = self.gensym("idx", 'int')
        forlist = self.gensym("list")
        listlen = self.gensym("iter", 'int')

        # first get the list
        forlist = self.compileExpression(node.list)

        self.append("%s = %s" % (listlen, forlist))
        self.append(loopidx + " = 0")

        # get the next item (also where "continue" jumps to)
        self.append(_for + ":")
        self.append("if %s >= %s goto %s" % (loopidx, listlen, _elsefor))

        # Okay: somewhere in our list we might call a generator.
        # Right now generators use parrot Coroutines. Coroutines
        # don't preserve the register stack. Without this save op,
        # the list we're looping through tends to get replaced
        # with the Coroutine. Other variables are probably also
        # being screwed up, but a "saveall" here would make an
        # infinite loop, so....
        self.append("save " + forlist)
        
        value = self.gensym("forval")
        self.append("%s = %s[%s]" % (value, forlist, loopidx))
        self.append(self.bindLocal(node.assign.name, value))
        self.append("%s = %s + 1" % (loopidx, loopidx))
        
        # do the loop body
        self.visit(node.body)

        # restore the list (see save note, above)
        self.append("restore " + forlist)

        # loop
        self.append("goto " + _for)

        # else: this is where we go if the loop ends with no "break"
        self.loops.pop() # no longer part of the loop
        self.append("%(_elsefor)s:" % locals())
        if node.else_:
            self.visit(node.else_)
            
        # end
        self.append(_endfor + ":")
        



    def visitBreak(self, node):
        assert self.loops, "break outside of loop" # SyntaxError
        self.append("goto %s" % self.loops[-1][1])


    def visitContinue(self, node):
        assert self.loops, "continue outside of loop" # SyntaxError
        self.append("goto %s" % self.loops[-1][0])


    def visitCallFunc(self, node):
        # visited when a function is called as a subroutine
        # (not as part of a larger expression or assignment)
        self.callingExpression(node, allocate=0)

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
        fun = self.genFunction(node, node.name, allocate=1)
        if self.classStack:
            klass = self.classStack[-1]
            self.append("setprop %s, '%s', %s" % (klass, node.name, fun))
            

    def visitReturn(self, node):
        raise SyntaxError, "return outside of function"

    def visitYield(self, node):
        raise SyntaxError, "yield outside of function"
        
                
    ##[ exceptions ]####################################################

    def visitRaise(self, node):
        assert node.expr1, "argument required for raise"
        assert not (node.expr2 or node.expr3), "only 1 arg alllowed for raise"
        self.set_lineno(node)
        msg = self.compileExpression(node.expr1)
        self.append("__py__raise(%s)" % msg)

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
        catch = self.genlabel("catch")
        handler = self.gensym("handler",'Sub')
        endtry = self.genlabel("endtry")
        self.append("newsub %s, .Exception_Handler, %s" \
                    % (handler, catch))
        self.append("set_eh " + handler)
        self.visit(node.body)
        self.append("clear_eh")
        self.append("goto " + endtry)
        self.append(catch + ":")
        for hand in node.handlers:
            expr, target, body = hand
            assert not (expr or target), \
                   "@TODO: can't get exception object yet"
            self.visit(body)
        self.append(endtry + ":")

                
        
    def visitTryFinally(self, node):
        # how do try/finally and continuations interact?
        # this is a hard question and should be brought up on the parrot list
        self.set_lineno(node)
        final = self.genlabel("final")
        handler = self.gensym("final",'Sub')
        self.append("newsub %s, .Exception_Handler, %s" % (handler, final))
        self.append("set_eh " + handler)
        self.visit(node.body)
        self.append("clear_eh")
        self.append(final + ":")
        self.visit(node.final) 


    def visitClass(self, node):
        name = node.name
        klass = self.gensym("klass", "object")
        
        # Python classes need to be callable, but ParrotClass
        # is not. So we make a constructor instead.
        # Classes with the same name will have identical
        # constructors, so no need to create several versions:

        if name not in self.classKnown:

            self.append("newclass %s, '%s'" % (klass, name))
            self.append("register %s" % klass)

            sub = []
            sub.append(".pcc_sub __new__%s non_prototyped" % name)
            sub.append("    .local object instance")
            sub.append("    find_type $I0, '%s'" % name)
            sub.append("    new instance, $I0")
            # calling conventions say P0 is this "ConstructorClass" sub
            # so this lets us do instance.__class__.__name__ ...
            sub.append("    setprop instance, '__class__', P0") 
            sub.append("    .pcc_begin_return")
            sub.append("        .return instance")
            sub.append("    .pcc_end_return")
            sub.append(".end") 
            self.classKnown.append(name)

            # Adds the new sub we just made to the subs list.
            # @TODO: mmm... smells like javascript (refactor me!)
            class Object: pass
            subObj = Object()
            subObj.getCode = lambda: "\n".join(sub)
            self.subs.append(subObj)

        # now make the constructor object:
        self.append("newsub %s, .Sub, __new__%s" % (klass, name))
        namesym = self.gensym("name")
        self.append(namesym + " = new PerlString")
        self.append("%s = '%s'" % (namesym, name))
        self.append("setprop %s, '__name__', %s" % (klass, namesym))
        self.append(self.bindLocal(name, klass))

        self.classStack.append(klass)
        self.visit(node.code)
        self.classStack.pop()
        

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
        self.locals = vars
        for arg in self.args:
            self.locals[arg]=arg
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
            res.append("    .param object " + arg)
        res.append("    new_pad %s" % self.depth) #@TODO: use -1 here??
        
        res.extend([self.bindLocal(arg, arg) for arg in self.args])
        res.extend(self.lines)
        res.append("    #------")
        res.append("    .local None none")
        res.append("    none=new None")
        res.append("    .pcc_begin_return")
        res.append("    .return none")
        res.append("    .pcc_end_return")
        res.append(".end")
        res.append("")
        return "\n".join(res)

    def getCodeForGenerator(self):
        
        stop = ast.Raise(ast.Const("StopIteration"), None, None)
        self.visit(stop)

        name = self.name
        res = []
        res.append(".pcc_sub %s non_prototyped" % name)
        res.append("   .local object gen_fun")
        res.append("   .local object gen_obj")
        res.append("   .local int iterator_type")

        res.append("   newsub gen_fun, .Coroutine, %s_g" % name)
        res.append('   find_type iterator_type, "PythonIterator"')
        res.append("   new gen_obj, iterator_type")
        res.append("   setprop gen_obj, 'next', gen_fun")

        res.append("   .pcc_begin_return")
        res.append("   .return gen_obj")
        res.append("   .pcc_end_return")
        res.append(".end")

        res.append(".pcc_sub %s_g non_prototyped" % name)
        res.append("   new_pad -1")
        res.extend(self.lines)
        res.append(".end")
        return "\n".join(res)
        
    #def assignToName(self, node, value):
    #    name = node.name
    #    if name in self.globals:
    #        self.append("store_lex  0, '%s', %s" % (name, value))
    #    else:
    #        self.locals[name] = name
    #        self.append(".local object _py_" + name)
    #        self.append("_py_%s = %s" % (name, value))


    def visitGlobal(self, node):
        for var in node.names:
            self.globals.append(var)

    def visitReturn(self, node):
        #@TODO: allow both yield and return in one function
        result = self.compileExpression(node.value, allocate=1)
        self.append("pop_pad")
        self.append(".pcc_begin_return")
        self.append(".return " + result)
        self.append(".pcc_end_return")

    def visitYield(self, node):
        self.isGenerator = 1
        result = self.compileExpression(node.value, allocate=1)
        next = self.gensym("next", "object")
        self.append("newsub %s, .Coroutine, label_%s" % (next,next))

        # P5 is the generator object. We know this because we
        # set it in getattr so we'd comply with the calling
        # conventions.
        self.append("setprop P5, 'next', %s" % next)

        self.append(".pcc_begin_yield")
        self.append(".return " + result)
        self.append(".pcc_end_yield")
        self.append("label_%s:" % next)

                
## module interface ###############################################

import time
HEAD=\
'''
# generated by pirate on %s
''' % time.asctime()
FOOT=\
'''
.include "pirate.imc"
.include "__builtin__.imc"
'''

def compile(src, name="__main__"):
    ast = compiler.parse(src)
    vis = compiler.visitor.ASTVisitor()
    pir = PirateVisitor(name)
    vis.preorder(ast, pir)
    pir.append("end")
    #@TODO: refactor this mess:
    if name=="__main__":
        lines = [".local object range",
                 "newsub range, .Sub, __builtin___range0",
                 "store_lex 0, 'range', range"]

        pir.lines = lines + pir.lines
    code =  pir.getCode()
    return code
                

def line_nos(seq):
    return [(i+1, seq[i]) for i in range(len(seq))]

def invoke(src, dump=0, lines=0):
    i,o = os.popen4("parrot --python -")
    code = compile(src)
    if dump:
        print
        if lines:
            for no, line in line_nos(code.split("\n")):
                print "% 4i: %s" % (no, line)
        else:
            print HEAD
            print code
            print FOOT
    print >> i, HEAD
    print >> i, code
    print >> i, FOOT
    i.close()    
    return o.read()

if __name__=="__main__":
    import os.path
    import sys

    #    if "-b" in sys.argv:
    src = open(os.path.join(sys.path[0],'__builtin__.py')).read()
    out = open(os.path.join(sys.path[0],"__builtin__.imc"),"w")
    print >> out, compile(src,"__builtin__")
    out.close()
    #print "rebuilt builtins.imc"
    
    if len(sys.argv) > 1:
        # file or stdin?
	if sys.argv[-1] == '-':
	    src = sys.stdin.read()
	else:
	    src = open(sys.argv[-1]).read()
        # dump or run?
        if "-d" in sys.argv:
            print HEAD
            print compile(src)
            print FOOT
        else:
            sys.stdout.write(invoke(src))
    else:
        print __doc__
        sys.exit()
        
