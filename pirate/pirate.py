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
    
    def append(self, line, indent=True):
        is_setline = line.startswith("setline")
        if indent:
            line = "    %-30s"  %line
            if not line.isspace():
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
        self.depth = depth or 0 # lexical scope depth
        self.klass = 0
        self.globals = {}
        self.classStack = [] # the class we're looking at right now
        self.reachable = True
        self.types = {}
        self.locals = {}
        self.seen_labels = []
        self.pending_unless = None

    def gensym(self, prefix="$P", type="object"):
        """
        Return a unique symbol for variable names in generated code.
        """
        self.counter.setdefault(prefix,-1)
        self.counter[prefix] += 1
        sym = "%s%i" % (prefix, self.counter[prefix]) 
        if type and not prefix.startswith("$"):  
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

    def find_type(self, type):
        if type in self.types:
            return self.types[type]
        index = self.gensym("$I")
        self.append("find_type %s, '%s'" % (index,type))
        self.types[type] = index
        return index

    def getCode(self):
        res  = ".sub %s @MAIN\n" % self.name
        res += "    new_pad 0\n"
        res += "\n".join(self.lines) + "\n"
        res += ".end\n"
        res += "\n\n".join([s.getCode() for s in self.subs]) + "\n"
        return res

    def set_lineno (self, node):
        if (node.lineno is not None and
            node.lineno != self._last_lineno):
            self._last_lineno = node.lineno
            self.append('setline %i' % node.lineno)


    def append(self, line, indent=True):
        if self.pending_unless:
            self.seen_labels.append(self.pending_unless[1])
            self.lines.append("unless %s goto %s" % self.pending_unless)
            self.pending_unless = None
        if self.reachable:
            self.lines.append(line, indent)

    def label(self, name, optional=False):
        if self.pending_unless:
            if self.pending_unless[1]!=name:
                self.seen_labels.append(self.pending_unless[1])
                self.append("unless %s goto %s" % self.pending_unless)
            self.pending_unless = None
        if optional and not name in self.seen_labels: return
        self.reachable = True
        self.append("%s:" % name, indent=False)
        self.types = {}
        self.locals = {}

    def unless(self, expression, label):
        if self.pending_unless:
            self.seen_labels.append(self.pending_unless[1])
            self.append("unless %s goto %s" % self.pending_unless)
        self.pending_unless = (expression, label)

    def goto(self, label):
        self.seen_labels.append(label)
        if self.pending_unless:
            cond = self.pending_unless[0]
            self.pending_unless = None
            self.append("if %s goto %s" % (cond, label))
        else:
            self.append("goto " + label)

    def bindLocal(self, name, value):
        self.locals[name] = value
        return "store_lex -1, '%s', %s" % (name, value)
    def bindGlobal(self, name, value):
        return "store_lex  0, '%s', %s" % (name, value)

    def lookupName(self, name):
        if name in self.locals:
            dest = self.locals[name]
            if not dest.startswith('$'):
                dest = self.gensym()
                self.append("%s = %s" % (dest, self.locals[name]))
                self.locals[name] = dest
        else:
            dest = self.gensym()
            self.append("find_lex %s, '%s'" % (dest, name))
            self.locals[name] = dest
        return dest
    ##[ expression compiler ]######################################
    
    def compileExpression(self, node, allocate=0):
        """
        In CPython, expression results just get pushed
        onto the stack. But since parrot uses registers
        instead, we need to to specify a destination 
        for each expression, and so we have to do our
        own dispatching outside the normal visitor walk.

        allocate==0  => don't allocate if constant
        allocate==1  => always allocate a var
        allocate==-1 => allocate a var if string 
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
            ast.Backquote:   self.backquote, 
            
            ast.Add: self.infixExpression,
            ast.Sub: self.infixExpression,
            ast.Mul: self.infixExpression,
            ast.Div: self.infixExpression,
            ast.Mod: self.infixExpression,
            ast.FloorDiv: self.infixExpression,
            ast.LeftShift: self.infixExpression,
            ast.RightShift: self.infixExpression,
            ast.Power: self.infixExpression,

            ast.Bitor: self.expressBitwise,
            ast.Bitxor: self.expressBitwise,            
            ast.Bitand: self.expressBitwise,

            ast.Slice: self.applySlice,

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
        str: "PyString",
        unicode: "PyString",
        int: "PyInt",
        float: "PyFloat",
        long: "PyLong",
        complex: "PyComplex",
    }


    def expressConstant(self, node, allocate):
        t = type(node.value)
        assert t in self.constMap, "unsupported const type:%s" % t
        r = repr(node.value)

        flag = ''
        if r.startswith("u'") or r.startswith('u"'):
           allocate = 1
           flag = 'u:'
           r=r[1:]

        if r.startswith("'") and r.endswith("'"):
            r = r.replace('"','\\"')
            r = '"' + r[1:-1] + '"'
            r = r.replace("\\'","'")

        if isinstance(node.value,complex): r = '"' + r + '"'
        if isinstance(node.value,long): r = '"' + r[:-1] + '"'

        if allocate>0 or (allocate==-1 and r.startswith('"')):
            dest = self.gensym()
            self.append("new %s, %s" % (dest,self.find_type(self.constMap[t])))
            self.append("%s = %s%s" % (dest, flag, r))
            return dest
        else:
            return r


    def expressDict(self, node, allocate):
        self.set_lineno(node)
        dest = self.gensym()
        self.append("new %s, %s" % (dest,self.find_type("PyDict")))
        if node.items:
            for k,v in node.items:
                key = self.compileExpression(k)
                val = self.compileExpression(v, allocate=1)
                self.append("%s[%s] = %s" % (dest, key, val))
        return dest



    def expressSubscript(self, node, allocate):
        base = self.compileExpression(node.expr, allocate=-1)

        for sub in node.subs[:-1]:
            temp = self.gensym()
            sub = self.compileExpression(sub, allocate=-1)
            self.append("%s = %s[%s]" % (temp, base, sub))
            base = temp

        sub = self.compileExpression(node.subs[-1], allocate=-1)
        
        slot = "%s[%s]" % (base, sub)

        if node.flags == "OP_DELETE":
            self.append("delete " + slot)
        elif node.flags=="OP_ASSIGN":
            return slot
        else:
            temp = self.gensym()
            self.append("%s=%s" % (temp, slot))
            return temp


    def expressGetattr(self, node, allocate):
        dest = self.gensym()
        attr = node.attrname

        self.set_lineno(node)
        obj = self.compileExpression(node.expr, allocate=1)
        self.append("getattribute %s, %s, '%s'" % (dest, obj, attr))

        return dest
    
        
    def nameExpression(self, node, allocate):
        dest = self.lookupName(node.name)
        return dest

    def listExpression(self, expr, allocate):
        dest = self.gensym()
        self.append("new %s, %s" % (dest,self.find_type("PyList")))
        for item in expr.nodes:
            sym = self.compileExpression(item)
            self.append("push %s, %s" % (dest, sym))
        return dest

    def tupleExpression(self, expr, allocate):
        dest = self.gensym()
        self.append("new %s, %s" % (dest,self.find_type("PyTuple")))
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

    def backquote(self, node, allocate):
        dest = self.gensym()
        temp = self.gensym("$S")
        value = self.compileExpression(node.expr, allocate=1)
        self.append("get_repr %s, %s" % (temp, value))
        self.append("new %s, %s" % (dest,self.find_type("PyString")))
        self.append("%s = %s" % (dest, temp))
        return dest

    def expressUnary(self, node, allocate):
        dest = self.gensym()
        value = self.compileExpression(node.expr, allocate=-1)
        op = self.unaryOps[node.__class__]
        self.append("new %s, %s" % (dest,self.find_type("PyObject")))
        self.append("%s = %s%s" % (dest, op, value))
        return dest


    bitwiseOps = {
        ast.Bitand: "&",
        ast.Bitor: "|",
        ast.Bitxor: "~", # weird but true
    }
    
    def expressBitwise(self, node, allocate):
        op = self.bitwiseOps[node.__class__]
        value = self.compileExpression(node.nodes[0], allocate=1)
        for n in node.nodes[1:]:
            next = self.compileExpression(n)
            tmp = self.gensym()
            self.append("new %s, %s" % (tmp,self.find_type("PyObject")))
            self.append("%s = %s %s %s" % (tmp, value, op, next))
            value = tmp
               
        return value

    infixOps = {
        ast.Add: "+",
        ast.Sub: "-",
        ast.Mul: "*",
        ast.Div: "/",
        ast.Mod: "%",
        ast.FloorDiv: "//",
        ast.RightShift: '>>',   # untested
        ast.LeftShift: '<<',    # untested
        ast.Power: "**",
    }
        
    def infixExpression(self, node, allocate):
        dest = self.gensym()
        self.append("new %s, %s" % (dest,self.find_type("PyObject")))
        lside = self.compileExpression(node.left, allocate=1)
        rside = self.compileExpression(node.right, allocate=-1)
        
        op = self.infixOps[node.__class__]
        self.append("%s = %s %s %s" % (dest, lside, op, rside))

        # put the result in our destination
        # (imcc seems to like this as a separate step
        # for typecasting or something...)
        #wtf?!
        #self.append("%s = %s" % (dest, result))
        return dest

    compOps = {
        "!=": "isne",
        "<":  "islt",
        "<=": "isle",
        "<>": "isne",
        "==": "iseq",
        ">":  "isgt",
        ">=": "isge",
        "is": "iseq",
        "is not": "isne",
    }

    def unlessExpression(self, test, label):
        "optimize simple if statements"

        if isinstance(test, ast.Compare):

            op, code = test.ops[0]
            if op=="is": op="=="
            if op=="is not": op="!="
            if op=="<>": op="!="
        
            if op<>"in":
                symL = self.compileExpression(test.expr, allocate=1)
                symR = self.compileExpression(code)
                self.unless("%s %s %s" % (symL, op, symR), label)
            else:
                testvar = self.compileExpression(test)
                self.unless(testvar, label)

        elif isinstance(test, ast.Const):
            if not test.value: 
                self.goto(label)

        else:
            testvar = self.compileExpression(test)
            self.unless(testvar, label)
        
    def compareExpression(self, expr, allocate):
        assert len(expr.ops) == 1, "@TODO: multi-compare not working yet"
        dest = self.gensym()
        # get left side:
        symL = self.compileExpression(expr.expr, allocate=1)

        # get the op:
        op, code = expr.ops[0]
        
        # get right side:
        symR = self.compileExpression(code, allocate=1)

        self.append("new %s, %s" % (dest,self.find_type("PyBoolean")))

        if op in self.compOps:
            temp = self.gensym("$I")
            self.append("%s %s, %s, %s" % (self.compOps[op], temp, symL, symR))
            self.append("%s = %s" % (dest, temp))
        else:
            assert op=="in"
            temp = self.gensym("$I")
            self.append("exists %s, %s[%s]" % (temp, symR, symL))
            self.append("%s = %s" % (dest, temp))

        return dest

    logicOps = {
        ast.Not: '!',
        ast.And: 'and',
        ast.Or: 'or',
    }


    def logicExpression(self, expr, allocate):
        operator = self.logicOps[expr.__class__]
        if operator == "!":            
            dest = self.gensym()
            tmp = self.compileExpression(expr.expr, allocate=1)
            self.append("new %s, %s" % (dest, self.find_type("PyBoolean")))
            self.append("not %s, %s" % (dest,tmp))
        else:
            dest = self.compileExpression(expr.nodes[0], allocate=1)
            for right in expr.nodes[1:]:
                tmp = self.compileExpression(right, allocate=1)
                self.append("%s %s, %s, %s" % (operator, dest, dest, tmp))
        return dest

    def expressSlice(self, lower, upper):
        lower = lower or ""
        if lower:
            lower = self.compileExpression(lower)
            if not lower.isdigit():
                temp = self.gensym("$I")
                self.append("%s = %s" % (temp, lower))
                lower = temp

        upper = upper or ""
        if upper:
            upper = self.compileExpression(upper)
            if not upper.isdigit():
                temp = self.gensym("$I")
                self.append("%s = %s" % (temp, upper))
                upper = temp

        return "%s .. %s" % (lower or "0", upper)

    def visitSlice(self, node):
        assert node.flags == "OP_DELETE"
        list = self.compileExpression(node.expr, allocate=-1)
        slice = self.expressSlice(node.lower, node.upper)
        self.append("delete %s[%s]" % (list,slice))

    def applySlice(self, node, allocate):
        assert node.flags == "OP_APPLY"
        list = self.compileExpression(node.expr, allocate=-1)
        slice = self.expressSlice(node.lower, node.upper)
        dest = self.gensym()
        self.append("%s = %s[%s]" % (dest,list,slice))
        return dest

    def assignSlice(self, node, value):
        assert node.flags == "OP_ASSIGN"
        list = self.compileExpression(node.expr, allocate=-1)
        slice = self.expressSlice(node.lower, node.upper)
        dest = self.gensym()
        self.append("%s[%s] = %s" % (list,slice,value))
        return dest

    def callingExpression(self, node, allocate):
        # gather up arguments
        args = []
        keywords = {}
        for arg in node.args:
            if isinstance(arg, ast.Keyword):
                keywords[arg.name] = self.compileExpression(arg.expr)
            else:
                args.append(self.compileExpression(arg, allocate=1))

        # figure out what we're calling
        if isinstance(node.node, ast.Lambda):
            sub = self.lambdaExpression(node.node, allocate=0)
        elif isinstance(node.node, ast.Getattr):
            obj = self.compileExpression(node.node.expr, allocate=1)
            sub = "%s.%s" % (obj,node.node.attrname)
        else:
            sub = self.compileExpression(node.node, allocate=1)

        if node.star_args or node.dstar_args or keywords:
            if node.star_args and not args:
                argx = self.compileExpression(node.star_args)
            else:
                argx = self.gensym()
                self.append("new %s, %s" % (argx,self.find_type("PyList")))

                for arg in args:
                    self.append("push %s, %s" % (argx, arg))

                if node.star_args:
                    sargs = self.compileExpression(node.star_args)
                    self.append("%s = %s + %s" % (argx, argx, sargs))

            if node.dstar_args and not keywords:
                keyx = self.compileExpression(node.dstar_args)
            else:
                keyx = self.gensym()
                self.append("new %s, %s" % (keyx,self.find_type("PyDict")))

                if node.dstar_args:
                    dsargs = self.compileExpression(node.star_args)
                    self.append("%s.update(%s)" % (keyx, dsargs))

                for (name,value) in keywords.items():
                    self.append("%s['%s']=%s" % (keyx, name, value))

            sub = sub + ".__call__"
            args = [argx,keyx]

        if allocate>-2:
            dest = self.gensym()
            self.append("%s=%s(%s)" % (dest,sub,",".join(args)))
        else:
            dest = ''
            self.append("%s(%s)" % (sub,",".join(args)))

        self.locals = {}
        return dest


    def lambdaExpression(self, node, allocate=0):
        return self.genFunction(node, None, method=False, allocate=0)


    def genFunction(self, node,  name, method=False, allocate=1):
        self.set_lineno(node)

        # functions are always anonymous, so make fake names
        sub = self.genlabel("_" + str(name))
        ref = self.gensym()

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
                               counter = None, # self.counter,
                               method = method,
                               args=node.argnames)

        # lambda is really just a single return function:
        if isLambda:            
            vis.preorder(ast.Return(node.code), pir)
        else:
            vis.preorder(node.code, pir)
            
        self.subs.append(pir)

        # store the address in dest
        self.append("newsub %s, %s, %s" % (ref, self.find_type("PyFunc"), sub))
        for (i, arg) in enumerate(node.argnames):
            self.append("%s[%s] = '%s'" % (ref, i, arg))

        if node.defaults:
            defaults = self.gensym()
            self.append("new %s, %s" % (defaults, self.find_type("PyList")))
            for (i,value) in enumerate(node.defaults):
                self.append("%s[%s] = %s" % 
                    (defaults, i, self.compileExpression(value)))
            self.append("setprop %s, 'func_defaults', %s" % (ref, defaults))

        if node.varargs: 
            self.append("setprop %s, 'func_varargs', %s" % (ref, ref))
        if node.kwargs: 
            self.append("setprop %s, 'func_kwargs', %s" % (ref, ref))

        if allocate>0:
            self.append(self.bindLocal(node.name, ref))
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
        dest = self.gensym()
        lcval = self.gensym()
        self.append("new %s, %s" % (dest,self.find_type("PyList")))
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
            arg = self.compileExpression(n)
            self.append("print_item %s" % arg)
            

    def visitPrintnl(self, node):
        self.visitPrint(node)
        self.append('print_newline')


    def visitIf(self, node):
        _endif = self.genlabel("endif")
        
        labels = [self.genlabel("elif") for t in node.tests[:-1]]
        if node.else_:
            labels.append(self.genlabel("else"))
        else:
            labels.append(_endif)
        labels.reverse()

        for test, body in node.tests:

            # if not true, goto _elif
            self.set_lineno(test)
            _elif = labels.pop()

            self.unlessExpression(test, _elif)
            
            # do it and goto _endif
            self.visit(body)
            if _elif <> _endif:
                self.goto(_endif)
            
            # _elif: (next test or pass through to else)
            self.label(_elif, optional=True)

        # else:
        if node.else_:
            self.set_lineno(node.else_)
            self.visit(node.else_)
            self.label(_endif, optional=True)


    ##[ assignment ]################################################

    def assign(self, node, value):
        """
        dispatch method for name/property/subscript
        """
        method = {
            ast.Subscript: self.assignToSubscript,
            ast.AssAttr: self.assignToProperty,
            ast.AssName: self.assignToName,
            ast.Slice: self.assignSlice,
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

        rside = listify(node.expr)

        for lside in node.nodes: 
            lside = listify(lside)

            if len(rside)==0 or len(lside)==1:
                # eg, x = []
                self.assign(lside[0],
                            self.compileExpression(node.expr, allocate=1))

            elif (len(lside) == len(rside)):
                ## this works for l=r AND l=r,r,r,r
                ## because of AssTuple nodes :)
                temp = []
                for expr in rside:
                    temp.append(self.compileExpression(expr, allocate=1))
                for i, node in enumerate(lside):
                    self.assign(node, temp[i])
    
            elif len(rside)==1:
                ## this handles l,l,l=r and l,l,l=r()
                rside = rside[0]
                if isinstance(rside, ast.Const):
                    ## @TODO: non-sequence check should be at *runtime*
                    raise TypeError("unpack non-sequence")
                else:
                    value = self.compileExpression(rside)
                    for (i, node) in enumerate(lside):
                        extract = self.gensym()
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

        ops = {'**=':'Power', '*=':'Mul', '/=':'Div', '//=':'FloorDiv',
               '%=':'Mod', '+=':'Add', '-=':'Sub', 
               '<<=':'LeftShift', '>>=':'RightShift',
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
        pad = self.gensym()
        name = node.name
        self.append("peek_pad " + pad)
        self.append("delete %s['%s']" % (pad, name))
        if name in self.locals: del self.locals[name]

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
        
        self.label(_while)
        testvar = self.compileExpression(node.test)
        if node.else_:
            self.unlessExpression(node.test, _elsewhile)
            self.visit(node.body)
            self.goto(_while)
            self.label(_elsewhile, optional=True)
            self.visit(node.else_)
        else:
            self.unlessExpression(node.test, _endwhile)
            self.visit(node.body)
            self.goto(_while)

        self.label(_endwhile, optional=True)
        self.loops.pop()


    def visitFor(self, node):
        self.set_lineno(node)
        _for = self.genlabel("for")
        _endfor = self.genlabel("endfor")
        _elsefor = self.genlabel("elsefor")
        self.loops.append((_for, _endfor))

        if not node.else_: _elsefor = _endfor

        # first get the list
        forlist = self.compileExpression(node.list, allocate=1)
        iter = self.gensym()
        self.append("%s = iter %s" % (iter, forlist))

        # get the next item (also where "continue" jumps to)
        item = self.gensym()
        self.label(_for)
        self.unless(iter, _elsefor)
        self.append("%s = shift %s" % (item, iter))

        if isinstance(node.assign, ast.AssTuple):
            # todo: throw something if len(node.assign.nodes) != len(item)
            for (i, name) in enumerate(node.assign.nodes):
                extract = self.gensym()
                self.append("%s = %s[%s]" % (extract, item, i))
                self.assign(name, extract)
        else:
            self.append(self.bindLocal(node.assign.name, item))
        
        # do the loop body
        self.visit(node.body)

        # loop
        self.goto(_for)

        # else: this is where we go if the loop ends with no "break"
        self.loops.pop() # no longer part of the loop
        if node.else_:
            self.label(_elsefor, optional=True)
            self.visit(node.else_)
            
        # end
        self.label(_endfor, optional=True)
        



    def visitBreak(self, node):
        assert self.loops, "break outside of loop" # SyntaxError
        self.goto(self.loops[-1][1])


    def visitContinue(self, node):
        assert self.loops, "continue outside of loop" # SyntaxError
        self.goto(self.loops[-1][0])


    def visitCallFunc(self, node):
        # visited when a function is called as a subroutine
        # (not as part of a larger expression or assignment)
        self.callingExpression(node, allocate=-2)

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
        if self.classStack:
            fun = self.genFunction(node, node.name, method=True, allocate=1)
            klass = self.classStack[-1]
            self.append("setprop %s, '%s', %s" % (klass, node.name, fun))
        else:
            fun = self.genFunction(node, node.name, method=False, allocate=1)
            

    def visitReturn(self, node):
        raise SyntaxError, "return outside of function"

    def visitYield(self, node):
        raise SyntaxError, "yield outside of function"
        
                
    ##[ exceptions ]####################################################

    def visitRaise(self, node):
        assert node.expr1, "argument required for raise"
        assert not (node.expr3), "only 3 arg raises not supported"
        self.set_lineno(node)

        exceptsym = self.gensym()

        if node.expr2:
            type = self.compileExpression(node.expr1)
            obj  = self.compileExpression(node.expr2)
            self.append("%s = %s()" % (exceptsym, type))
            self.append("setref %s, %s" % (exceptsym,obj))
        else:
            msg = self.compileExpression(node.expr1)
            self.append("new %s, .Exception" % exceptsym)
            self.append('%s["_message"] = %s' % (exceptsym, msg))

        self.append("throw %s" % exceptsym)

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
        endtry = self.genlabel("endtry")
        self.append("push_eh " + catch)
        self.visit(node.body)
        self.append("clear_eh")
        self.goto(endtry)
        self.label(catch)
        for hand in node.handlers:
            expr, target, body = hand
            if expr:
                expr = self.compileExpression(expr) + ".__match__(P5)"
                if target:
                    tmp = self.gensym()
                    self.append("%s = %s" % (tmp,expr))
                    self.assign(target,tmp)
                else:
                    self.append(expr)
            self.visit(body)
        self.label(endtry, optional=True)

                
        
    def visitTryFinally(self, node):
        # how do try/finally and continuations interact?
        # this is a hard question and should be brought up on the parrot list
        self.set_lineno(node)
        final = self.genlabel("final")
        handler = self.gensym()
        self.append("newsub %s, .Exception_Handler, %s" % (handler, final))
        self.append("set_eh " + handler)
        self.visit(node.body)
        self.append("clear_eh")
        self.label(final)
        self.visit(node.final) 


    def visitClass(self, node):
        name = node.name
        super = self.gensym()
        klass = self.gensym()
        genname = self.genlabel("_" + str(name))
        
        self.append("getclass %s, 'PyClass'" % super)
        self.append("subclass %s, %s, '%s'" % (klass, super, genname))

        # now set the name attribute
        namesym = self.gensym()
        self.append("new %s, %s" % (namesym,self.find_type("PyString")))
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
    def __init__(self, name, depth, doc, counter, method, args=[]):
        super(PirateSubVisitor, self).__init__(name, counter=counter)
        self.doc = doc
        self.args = args
        self.method = method
        if self.method and self.args:
            self.locals[self.args[0]] = "P2"
            for (i,arg) in enumerate(self.args[1:]):
                self.locals[arg] = "P%d" % (i+5)
        else:
            for (i,arg) in enumerate(self.args):
                self.locals[arg] = "P%d" % (i+5)
        self.counter = counter or {}
        self.depth = depth
        self.globals = []
        self.isGenerator = 0
        self.emptyReturn = None
        
    def getCode(self):
        if self.isGenerator:
            res = self.getCodeForGenerator()
        else:
            res = self.getCodeForFunction()
        return res + "\n\n".join([s.getCode() for s in self.subs])

    def getCodeForFunction(self):
        code = self.lines
        self.lines = imclist()
        fallthrou = self.reachable
        self.reachable = True

        if self.doc:
            self.append("")
            self.append("# %s" % self.doc, indent=False)
        self.append(".sub %s @ANON" % self.name, indent=False)
        self.append("new_pad %s" % self.depth) #@TODO: use -1 here??
        
        if self.method and self.args:
            self.append(self.bindLocal(self.args[0], "P2"))
            for (i,arg) in enumerate(self.args[1:]):
                self.append(self.bindLocal(arg, "P%d" % (i+5)))
        else:
            for (i,arg) in enumerate(self.args):
                self.append(self.bindLocal(arg, "P%d" % (i+5)))
        self.lines.extend(code)

        if self.emptyReturn:
            fallthru = True
            self.label(self.emptyReturn, optional=True)

        if fallthrou:
            dest = self.gensym()
            self.append("new %s, %s" % (dest,self.find_type("PyNone")))
            self.append(".return (%s)" % dest)
        self.append(".end", indent=False)
        self.append("")
        return "\n".join(self.lines)

    def getCodeForGenerator(self):
        code = self.lines
        self.lines = imclist()
        fallthrou = self.reachable
        self.reachable = True
        
        gen = self.gensym()
        result = self.gensym()

        name = self.name
        if self.doc:
            self.append("")
            self.append("# %s" % self.doc, indent=False)

        self.append(".sub \"%s\" @ANON" % name, indent=False)
        label = self.genlabel("gen")

        self.append("newsub %s, .Coroutine, %s" % (gen,label))
        self.append("new %s, %s" % (result,self.find_type("PyGen")))
        self.append("setref %s, %s" % (result,gen))

        self.append(".return (%s)" % result)
        self.label(label)
        self.append("new_pad -1")
        self.lines.extend(code)

        if self.emptyReturn:
            fallthru = True
            self.label(self.emptyReturn, optional=True)

        if fallthrou:
            stop = ast.Raise(ast.Const("StopIteration"), None, None)
            self.visit(stop)

        self.append(".end", indent=False)
        return "\n".join(self.lines)
        
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
        if isinstance(node.value,ast.Const) and node.value.value == None:
            self.emptyReturn = self.genlabel("return")
            self.goto(self.emptyReturn)
        else:
            result = self.compileExpression(node.value, allocate=1)
            self.append("pop_pad")
            self.append(".return (" + result + ")")
            self.reachable = False

    def visitYield(self, node):
        self.isGenerator = 1
        result = self.compileExpression(node.value, allocate=1)

        self.append(".yield (" + result + ")")

                
## module interface ###############################################

import time
HEAD=\
'''
# generated by pirate on %s
''' % time.asctime()
FOOT=""

def compile(src, name="__main__"):
    ast = compiler.parse(src)
    vis = compiler.visitor.ASTVisitor()
    pir = PirateVisitor(name)
    vis.preorder(ast, pir)
    if name=="__main__":
        pre  = ["    loadlib P1, 'python_group'",
                "    find_global P0, 'PyBuiltin', '__load__'",
                "    invoke",
                "    push_eh __py_catch"]
        post = ["    .return ()",
                "__py_catch:",
                "    print_item P5",
                '    print_newline']
        pir.lines = pre + ["#"] + pir.lines + ["#"] + post
    code =  pir.getCode()
    return code
                

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
        
