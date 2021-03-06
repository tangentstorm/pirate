#!/usr/bin/python
"""
pirate: python->parrot compiler
 usage: pirate.py [-d] [-t] filename.py
        -d dumps the generated parrot code
        -t traces the execution of the parrot code
"""

# TODO: change imclist to be aware of var/const differences, take
# instructions and values -- eventually be essentially an AST

# dump use of scratchpads entirely, work out proper scheme for nested
# funs, use a dict for globals, put locals in named temps, un-name
# real temps

# do type inference to unbox locals.

from __future__ import generators
from types import GeneratorType
import os
import compiler
import traceback
import simple
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
        #this check allows the emit()s to work     
        if line is not None:
            if isinstance(line, GeneratorType):
                tmp = ""
                for item in line:
                    tmp += str(item)
                line = tmp    
            if isinstance(line, tuple):
                line = line[0]
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
    
    def __init__(self, name, depth=None):
        self.name = name
        self._last_lineno = None
        self.lines = imclist()
        self.loops = []
        self.counter = {}
        self.subs = []
        self.depth = depth or 0 # lexical scope depth
        self.klass = 0
        self.globals = {}
        self.classStack = [] # the class we're looking at right now
        self.reachable = True
        #self.types = {}
        self.locals = {}
        self.seen_labels = []
        self.pending_unless = None
        self.exception = None
        self.listcomps = []  # list comprehension temp lists

    def gensym(self, prefix="$P", type="object"):
        """
        Return a unique symbol for variable names in generated code.
        """
        self.counter.setdefault(prefix,-1)
        self.counter[prefix] += 1
        sym = "%s%i" % (prefix, self.counter[prefix]) 
        if type and not prefix.startswith("$"):  
            #self.append (".local %s %s" % (type, sym))
            return (".local %s %s" % (type, sym), sym)
        return (None,sym)

    def genlabel(self, prefix):
        """
        Return a unique symbol for label names in generated code.
        """
        self.counter.setdefault(prefix,-1)
        self.counter[prefix] += 1
        sym = "%s%i" % (prefix, self.counter[prefix]) 
        return sym

    def find_type(self, type):
        return "'%s'" % type
        # if type in self.types:
        #    return self.types[type]
        #index = self.gensym("$I")
        #self.append("find_type %s, '%s'" % (index,type))
        #self.types[type] = index
        #return index

    def flatten(self, series):
        """
        take a list that might contain other lists or
        generators  and flatten it so that it's just
        one big list
        """
        for thing in series:
            if type(thing) in [list, GeneratorType]:
                for sub in flatten(thing):
                    yield sub
            else:
                yield thing
                
    def getLines(self):
        return self.flatten(self.lines)                

    def getCode(self):
	res  = ".sub %s :main\n" % self.name
        #res += "    new_pad 0\n"
        res += "\n".join(self.getLines()) + "\n"#list(self.getLines())) + "\n"
        res += ".end\n"
        res += "\n\n".join([s.getCode() for s in self.subs]) + "\n"
        return res

    def set_lineno (self, node):
        if (node.lineno is not None and
            node.lineno != self._last_lineno):
            self._last_lineno = node.lineno
            #self.append('setline %i' % node.lineno)
            return ('setline %i' % node.lineno)
            


    def append(self, line, indent=True):
        #text1, text2, text3 = None
        if self.pending_unless:
            self.seen_labels.append(self.pending_unless[1])
            #text1 = self.pending_unless[1]
            self.lines.append("unless %s goto %s" % self.pending_unless)
            #text2 = ("unless %s goto %s" % self.pending_unless) 
            self.pending_unless = None
        if self.reachable:
            self.lines.append(line, indent)
            #text3 = line, indent
        #return text1, text2, text3

    def label(self, name, optional=False):
        selflinesappend = None
        selfappend = None
        if self.pending_unless:
            if self.pending_unless[1]!=name:
                self.seen_labels.append(self.pending_unless[1])
                #self.lines.append("unless %s goto %s" % self.pending_unless)
                selflinesappend = "unless %s goto %s" % self.pending_unless
            self.pending_unless = None
        if optional and not name in self.seen_labels: 
            return selflinesappend, None, None
        self.reachable = True
        #self.append("%s:" % name, indent=False)
        selfappend = "%s:" % name        
        #self.types = {}
        self.locals = {}
        indent = False
        return selflinesappend, selfappend, indent

    def unless(self, expression, label):
        text = None
        if self.pending_unless:
            self.seen_labels.append(self.pending_unless[1])
            #self.append("unless %s goto %s" % self.pending_unless)
            text =  "unless %s goto %s" % self.pending_unless
        self.pending_unless = (expression, label)
        return text
        
    def goto(self, label):
        if not self.reachable: return None, None
        self.seen_labels.append(label)
        if self.pending_unless:
            cond = self.pending_unless[0]
            self.pending_unless = None
            #self.append("if %s goto %s" % (cond, label))
            return "if %s goto %s" % (cond, label), 1
        else:
            
            #self.append("goto " + label)
            #self.reachable=False
            return "goto " + label, 0
            #self.append("goto " + label)
            

    def bindLocal(self, name, value):
        self.locals[name] = value
	return "store_global '%s', %s" % (name, value) # XXX: should be store_lex
    def bindGlobal(self, name, value):
        return "store_global '%s', %s" % (name, value)

    def lookupName(self, name):
        text = ""
        if name in self.locals:
            dest = self.locals[name]
            if not dest.startswith('$'):
                regtext, dest = self.gensym()
                if regtext is not None:
                    self.append(regtext)
                text = "%s = %s" % (dest, self.locals[name])
                #self.append("%s = %s" % (dest, self.locals[name]))
                self.locals[name] = dest
        else:
            regtext, dest = self.gensym()
            if regtext is not None:
                self.append(regtext)
	    text = "find_global %s, '%s'" % (dest,name) # XXX: should be find_lex
	   #self.append("find_lex %s, '%s'" % (dest, name))
            self.locals[name] = dest
        return dest, text
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


            simple.SimpleListComp: self.expressSimpleListComp,
            

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
           flag = 'binary:'
           r=r[1:]

        if r.startswith("'") and r.endswith("'"):
            r = r.replace('"','\\"')
            r = '"' + r[1:-1] + '"'
            r = r.replace("\\'","'")

        if isinstance(node.value,complex): r = '"' + r + '"'
        if isinstance(node.value,long): r = '"' + r[:-1] + '"'

        if allocate>0 or (allocate==-1 and r.startswith('"')):
            regtext, dest = self.gensym()
            if regtext is not None:
                self.append(regtext)            
            self.append("new %s, %s" % (dest,self.find_type(self.constMap[t])))
            self.append("%s = %s%s" % (dest, flag, r))
            return dest
        else:
            return r


    def expressDict(self, node, allocate):
        self.append(self.set_lineno(node))
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
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
            regtext, temp = self.gensym()
            if regtext is not None:
                self.append(regtext)
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
            regtext, temp = self.gensym()
            if regtext is not None:
                self.append(regtext)
            self.append("%s=%s" % (temp, slot))
            return temp


    def expressGetattr(self, node, allocate):
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
        attr = node.attrname

        self.append(self.set_lineno(node))
        obj = self.compileExpression(node.expr, allocate=1)
        self.append("getattribute %s, %s, '%s'" % (dest, obj, attr))

        return dest
    
        
    def nameExpression(self, node, allocate):
        dest, text = self.lookupName(node.name)
        self.append(text)
        return dest

    def listExpression(self, expr, allocate):
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
        self.append("new %s, %s" % (dest,self.find_type("PyList")))
        for item in expr.nodes:
            sym = self.compileExpression(item)
            self.append("push %s, %s" % (dest, sym))
        return dest

    def tupleExpression(self, expr, allocate):
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
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
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
        regtext, temp = self.gensym("$S")
        if regtext is not None:
            self.append(regtext)        
        value = self.compileExpression(node.expr, allocate=1)
        self.append("get_repr %s, %s" % (temp, value))
        self.append("new %s, %s" % (dest,self.find_type("PyString")))
        self.append("%s = %s" % (dest, temp))
        return dest

    def expressUnary(self, node, allocate):
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
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
            next = self.compileExpression(n, allocate=-1)
            regtext, tmp = self.gensym()
            if regtext is not None:
                self.append(regtext)
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
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
        self.append("new %s, %s" % (dest,self.find_type("PyObject")))
        lside = self.compileExpression(node.left, allocate=1)
        rside = self.compileExpression(node.right, allocate=-1)
        
        op = self.infixOps[node.__class__]
        self.append("%s = %s %s %s" % (dest, lside, op, rside))

        return dest

    compOps = {
        "!=": "isne",
        "<":  "islt",
        "<=": "isle",
        "<>": "isne",
        "==": "iseq",
        ">":  "isgt",
        ">=": "isge",
        "is": "issame",
        "is not": "isntsame",
    }

    def unlessExpression(self, test, label):
        "optimize simple if statements"
        appendedCode = []
        appendedLines = []
        if isinstance(test, ast.Compare):

            op, code = test.ops[0]
            if op=="<>": op="!="
        
            if op not in ("in","not in","is","is not"):
                symL = self.compileExpression(test.expr, allocate=1)
                symR = self.compileExpression(code)
                #self.append(self.unless("%s %s %s" % (symL, op, symR), label))
                appendedCode.append(self.unless("%s %s %s" % (symL, op, symR), label))
            else:
                testvar = self.compileExpression(test)
                #self.append(self.unless(testvar, label))
                appendedCode.append(self.unless(testvar, label))

        elif isinstance(test, ast.Const):
            if not test.value:
                text, tf = self.goto(label) 
                appendedCode.append([text,tf])
                #if not tf:
                #    self.reachable = false 

        elif isinstance(test, ast.And):
            for node in test.nodes:
                self.unlessExpression(node, label)

        elif isinstance(test, ast.Or):
            # this looks convoluted but optimizes down nicely
            thenlabel = self.genlabel("then")
            for node in test.nodes[:-1]:
                orlabel = self.genlabel("or")
                self.unlessExpression(node, orlabel)
                text, tf = self.goto(thenlabel)
                appendedCode.append(text, tf)
                #if not tf:
                #    self.reachable = False
                #self.label(orlabel, optional=True)
                selflinesappend, selfappend, indent = self.label(orlabel, optional=True)
                if selflinesappend is not None:
                    appendedLines.append(selflinesappend)
                if selfappend is not None:
                    appendedCode.append(selfappend, indent)
            self.unlessExpression(test.nodes[-1], label)
            selflinesappend, selfappend, indent = self.label(thenlabel, optional=True)
            if selflinesappend is not None:
                appendedlines.append(selflinesappend)
            if selfappend is not None:
                appendedCode.append(selfappend, indent)
            #self.label(thenlabel, optional=True)

        else:
            testvar = self.compileExpression(test)
            #self.append(self.unless(testvar, label))
            appendedCode.append(self.unless(testvar, label))
        
        return appendedCode, appendedLines
        
    def compareExpression(self, expr, allocate):
        assert len(expr.ops) == 1, "@TODO: multi-compare not working yet"
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
        # get left side:
        symL = self.compileExpression(expr.expr, allocate=1)

        # get the op:
        op, code = expr.ops[0]
        
        # get right side:
        symR = self.compileExpression(code, allocate=1)

        self.append("new %s, %s" % (dest,self.find_type("PyBoolean")))

        if op in self.compOps:
            regtext, temp = self.gensym("$I")
            if regtext is not None:
                self.append(regtext)            
            self.append("%s %s, %s, %s" % (self.compOps[op], temp, symL, symR))
            self.append("%s = %s" % (dest, temp))
        else:
            assert op in ["in","not in"]
            regtext, temp = self.gensym("$I")
            if regtext is not None:
                self.append(regtext)
            self.append("exists %s, %s[%s]" % (temp, symR, symL))
            self.append("%s = %s" % (dest, temp))
            if op == "not in":
                self.append("not %s, %s" % (dest, dest))

        return dest

    logicOps = {
        ast.Not: '!',
        ast.And: 'and',
        ast.Or: 'or',
    }


    def logicExpression(self, expr, allocate):
        operator = self.logicOps[expr.__class__]
        if operator == "!":            
            regtext, dest = self.gensym()
            if regtext is not None:
                self.append(regtext)
            tmp = self.compileExpression(expr.expr, allocate=1)
            self.append("new %s, %s" % (dest, self.find_type("PyBoolean")))
            self.append("not %s, %s" % (dest,tmp))
        else:
            dest = self.compileExpression(expr.nodes[0], allocate=1)
            label = self.genlabel("shortcircuit")
            for right in expr.nodes[1:]:
                if operator=='and':
                    self.append(self.unless(dest, label))
                else:
                    orlabel = self.genlabel("or")
                    self.append(self.unless(dest, orlabel))                    
                    text, tf = self.goto(label)
                    self.append(text)
                    if not tf:
                        self.reachable = False
                    selflinesappend, selfappend, indent = self.label(orlabel)
                    if selflinesappend is not None:
                        self.lines.append(selflinesappend)
                    if selfappend is not None:
                        self.append(selfappend, indent)        
                    #self.goto(label)
                    #self.label(orlabel)
                tmp = self.compileExpression(right, allocate=1)
                self.append("%s = %s" % (dest, tmp))
            selflinesappend, selfappend, indent = self.label(label)
            if selflinesappend is not None:
                self.lines.append(selflinesappend)
            if selfappend is not None:
                self.append(selfappend, indent)        

            #self.label(label)
        return dest

    def expressSlice(self, lower, upper):
        lower = lower or ""
        if lower:
            lower = self.compileExpression(lower)
            if not lower.isdigit():
                regtext, temp = self.gensym("$I")
                if regtext is not None:
                    self.append(regtext)
                self.append("%s = %s" % (temp, lower))
                lower = temp

        upper = upper or ""
        if upper:
            upper = self.compileExpression(upper)
            if not upper.isdigit():
                regtext, temp = self.gensym("$I")
                if regtext is not None:
                    self.append(regtext)
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
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
        self.append("%s = %s[%s]" % (dest,list,slice))
        return dest

    def assignSlice(self, node, value):
        assert node.flags == "OP_ASSIGN"
        list = self.compileExpression(node.expr, allocate=-1)
        slice = self.expressSlice(node.lower, node.upper)
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
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
                regtext, argx = self.gensym()
                if regtext is not None:
                    self.append(regtext)                
                self.append("new %s, %s" % (argx,self.find_type("PyList")))

                for arg in args:
                    self.append("push %s, %s" % (argx, arg))

                if node.star_args:
                    sargs = self.compileExpression(node.star_args)
                    self.append("%s = %s + %s" % (argx, argx, sargs))

            if node.dstar_args and not keywords:
                keyx = self.compileExpression(node.dstar_args)
            else:
                regtext, keyx = self.gensym()
                if regtext is not None:
                    self.append(regtext)
                self.append("new %s, %s" % (keyx,self.find_type("PyDict")))

                if node.dstar_args:
                    dsargs = self.compileExpression(node.star_args)
                    self.append("%s.update(%s)" % (keyx, dsargs))

                for (name,value) in keywords.items():
                    self.append("%s['%s']=%s" % (keyx, name, value))

            if sub.find('.')>=0:
                regtext, tmp = self.gensym()
                if regtext is not None:
                    self.append(regtext)                
                args = tuple([tmp]+sub.split('.'))
                self.append("getattribute %s, %s, '%s'" % args)
                sub = tmp

            args = [argx,keyx]
            sub = sub + ".__call__"

        if allocate>-2:
            regtext, dest = self.gensym()
            if regtext is not None:
                self.append(regtext)
            self.append("%s=%s(%s)" % (dest,sub,",".join(args)))
        else:
            dest = ''
            self.append("%s(%s)" % (sub,",".join(args)))

        self.locals = {}
        return dest


    def lambdaExpression(self, node, allocate=0):
        return self.genFunction(node, None, allocate=0)


    def genFunction(self, node,  name, allocate=1):
        self.append(self.set_lineno(node))

        # functions are always anonymous, so make fake names
        sub = self.genlabel("_" + str(name))
        regtext, ref = self.gensym()
        if regtext is not None:
            self.append(regtext)

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
                               args=node.argnames)

        # lambda is really just a single return function:
        if isLambda:            
            vis.preorder(ast.Return(node.code), pir)
        else:
            vis.preorder(node.code, pir)
            if pir.isGenerator:
                # Python generators return, so parameters aren't local
                pir = PirateSubVisitor(sub,
                               depth = self.depth+1,
                               doc=comment,
                               args=node.argnames)
                pir.locals={}
                vis = compiler.visitor.ASTVisitor()
                vis.preorder(node.code, pir)
            
        self.subs.append(pir)

        # store the address in dest
        self.append("new %s, %s" % (ref, self.find_type("PyFunc")))
        #self.append("set_addr %s, %s" % (ref, sub))
	self.append("find_global %s, '%s'" % (ref, sub))
        if isLambda: 
            self.append("set %s, '<lambda>'" % ref)
        elif name: 
            self.append("set %s, '%s'" % (ref, name))
	# FIXME: took this out for now because pmc couldn't handle it
	# need to implement set_string in pyfunc.pmc I believe
        #for (i, arg) in enumerate(node.argnames):
        #    self.append("%s[%s] = '%s'" % (ref, i, arg))

        if node.defaults:
            regtext, defaults = self.gensym()
            if regtext is not None:
                self.append(regtext)            
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

    def expressSimpleListComp(self, node, allocate):
        # emit code:###############################
        self.append(self.set_lineno(node))
        regtext, dest = self.gensym()
        if regtext is not None:
            self.append(regtext)
        self.listcomps.append(dest)        
        ### ??? lcval = self.gensym()
        # here we create our anonymous list:
        self.append("new %s, %s" % (dest,self.find_type("PyList")))
        self.visit(node.forLoop)
        self.listcomps.pop()
        return dest

    def visitListCompCore(self, node):
        #print len(self.listcomps)
        pmc = self.listcomps[-1]
        exp = self.compileExpression(node.expr)
        self.append("push %s, %s" % (pmc, exp))
        

    ##[ visitor methods ]##########################################

    def visitPrint(self, node):
        dest = None
        if node.dest: dest = self.compileExpression(node.dest)
        for n in node.nodes:
            arg = self.compileExpression(n)
            if dest:
                self.append("print_item %s,%s" % (dest,arg))
            else:
                self.append("print_item %s" % arg)
            

    def visitPrintnl(self, node):
        self.visitPrint(node)
        if node.dest:
            self.append('print_newline %s', self.compileExpression(node.dest))
        else:
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
            self.append(self.set_lineno(test))
            _elif = labels.pop()
            appendedCode, appendedLines = self.unlessExpression(test, _elif)
            if len(appendedCode) > 0:
                for item in appendedCode:
                    if isinstance(item, list):
                        self.append(item[0])
                        if not item[1]:      
                            self.reachable = False
                    else:
                        self.append(item)
            if len(appendedLines) > 0:
                for item in appendedLines:
                    self.lines.append(item)         

            #self.unlessExpression(test, _elif)
            
            # do it and goto _endif
            self.visit(body)
            if _elif <> _endif:
                text, tf = self.goto(_endif)
                self.append(text)
                if not tf:
                    self.reachable = False
                
                #self.append(self.goto(_endif))
            
            # _elif: (next test or pass through to else)
            selflinesappend, selfappend, indent = self.label(_elif, optional=True)
            if selflinesappend is not None:
                self.lines.append(selflinesappend)
            if selfappend is not None:
                self.append(selfappend, indent)

            #self.label(_elif, optional=True)

        # else:
        if node.else_:
            self.append(self.set_lineno(node.else_))
            self.visit(node.else_)
            selflinesappend, selfappend, indent = self.label(_endif, optional=True)
            if selflinesappend is not None:
                self.lines.append(selflinesappend)
            if selfappend is not None:
                self.append(selfappend, indent)

            #self.label(_endif, optional=True)


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

        lsides = []
        for names in node.nodes:
            lsides.insert(0, listify(names))

        rside = []
        expr = listify(node.expr)
        if len(expr)==0 or min(map(len,lsides))==1:
            # eg, x = []
            rside.append(self.compileExpression(node.expr, allocate=1))
        else:
            for value in expr:
                rside.append(self.compileExpression(value, allocate=1))

        for lside in lsides:

            if len(lside)==1:
                # eg, x = []
                self.assign(lside[0], rside[0]);

            elif (len(lside) == len(rside)):
                ## this works for l=r AND l=r,r,r,r
                ## because of AssTuple nodes :)
                for i, node in enumerate(lside):
                    self.assign(node, rside[i])
    
            elif len(rside)==1:
                ## this handles l,l,l=r and l,l,l=r()
                ## @TODO: compare len(lside) against rside.elements()
                for (i, node) in enumerate(lside):
                    regtext, extract = self.gensym()
                    if regtext is not None:
                        self.append(regtext)                    
                    self.append("%s = %s[%s]" % (extract, rside[0], i))
                    self.assign(node, extract)
            else:
                ## i don't THINK there are any other combinations... (??)
                ## @TODO: unpack wrong size check should also be at runtime
                raise ValueError("unpack sequence of wrong size")

    ##[ augmented assign ]#########################################

    def visitAugAssign(self, aug):
        """tranforms tree to a standard assignment and calls visitAssign
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
        when node.flags == OP_DELETE, it means 'del name'
        
        when node.flags == OP_ASSIGN, it's an assignment but
        as far as I can tell this only actually happens in
        list comprehensions, eg: lexical 'i' in '[i for i in (1,2,3)]'
        """
        if node.flags == "OP_DELETE":
            "expected AssName to be a del!"
            regtext, pad = self.gensym()
            if regtext is not None:
                self.append(regtext)            
            name = node.name
            #self.append("peek_pad " + pad)
            self.append("delete %s['%s']" % (pad, name))
            if name in self.locals: del self.locals[name]
        elif node.flags=="OP_ASSIGN":
            #@TODO: handle this for list comprehensions
            pass
        else:
            raise NotImplementedError("can't handle %s yet" % node.flags)


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
    def visitPassEmitter(self, node):
        self.lines.append(node.emit())
#SETH
    def visitWhileEmitter(self, node):
        self.lines.append(node.emit())
        #print "************" + str(self.lines) + "&&&&&&&&&"
        
    def visitWhile(self, node):        
        self.append(self.set_lineno(node))
        _while = self.genlabel("while")
        #print "_while: " + str(_while)
        _elsewhile = self.genlabel("elsewhile") # sync nums even if no "else"
        #print "_elsewhile: " + str(_elsewhile)
        _endwhile = self.genlabel("endwhile")
        #print "_endwhile: " + str(_endwhile)
        self.loops.append((_while, _endwhile))
        
        #self.label(_while)
        selflinesappend, selfappend, indent = self.label(_while)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)
        if node.else_:
            appendedCode, appendedLines = self.unlessExpression(node.test, _elsewhile)
            if len(appendedCode) > 0:
                for item in appendedCode:
                    if isinstance(item, list):
                        self.append(item[0])
                        if not item[1]:      
                            self.reachable = False
                    else:
                        self.append(item)
            if len(appendedLines) > 0:
                for item in appendedLines:
                    self.lines.append(item)         
            self.visit(node.body)
            text, tf = self.goto(_while)
            self.append(text)
            if not tf:
                self.reachable = False
            selflinesappend, selfappend, indent = self.label(_elsewhile, optional=True)
            if selflinesappend is not None:
                self.lines.append(selflinesappend)
            if selfappend is not None:
                self.append(selfappend, indent)            
            self.visit(node.else_)
        else:
            appendedCode, appendedLines = self.unlessExpression(node.test, _endwhile)
            if len(appendedCode) > 0:
                for item in appendedCode:
                    if isinstance(item, list):
                        self.append(item[0])
                        if not item[1]:      
                            self.reachable = False
                    else:
                        self.append(item)
            if len(appendedLines) > 0:
                for item in appendedLines:
                    self.lines.append(item)
            self.visit(node.body)
            text, tf = self.goto(_while)
            self.append(text)
            if not tf:
                self.reachable = False
        selflinesappend, selfappend, indent = self.label(_endwhile, optional=True)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)        
        self.loops.pop()


    def visitFor(self, node):
        self.append(self.set_lineno(node))
        _for = self.genlabel("for")
        _endfor = self.genlabel("endfor")
        _elsefor = self.genlabel("elsefor")
        self.loops.append((_for, _endfor))

        if not node.else_: _elsefor = _endfor

        # first get the list
        forlist = self.compileExpression(node.list, allocate=1)
        regtext, iter = self.gensym()
        if regtext is not None:
            self.append(regtext)
        self.append("%s = iter %s" % (iter, forlist))

        # get the next item (also where "continue" jumps to)
        regtext, item = self.gensym()
        if regtext is not None:
            self.append(regtext)        
        selflinesappend, selfappend, indent = self.label(_for)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)
        #self.label(_for)
        self.append(self.unless(iter, _elsefor))
        self.append("%s = shift %s" % (item, iter))

        if isinstance(node.assign, ast.AssTuple):
            # todo: throw something if len(node.assign.nodes) != len(item)
            for (i, name) in enumerate(node.assign.nodes):
                regtext, extract = self.gensym()
                if regtext is not None:
                    self.append(regtext)                
                self.append("%s = %s[%s]" % (extract, item, i))
                self.assign(name, extract)
        else:
            self.append(self.bindLocal(node.assign.name, item))
        
        # do the loop body
        self.visit(node.body)

        # loop
        text, tf = self.goto(_for)
        self.append(text)
        if not tf:
            self.reachable = False
        
        #self.append(self.goto(_for))

        # else: this is where we go if the loop ends with no "break"
        self.loops.pop() # no longer part of the loop
        if node.else_:
            selflinesappend, selfappend, indent = self.label(_elsefor, optional=True)
            if selflinesappend is not None:
                self.lines.append(selflinesappend)
            if selfappend is not None:
                self.append(selfappend, indent)        
            
            #self.label(_elsefor, optional=True)
            self.visit(node.else_)
            
        # end
        selflinesappend, selfappend, indent = self.label(_endfor, optional=True)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)        

        #self.label(_endfor, optional=True)
        



    def visitBreak(self, node):
        assert self.loops, "break outside of loop" # SyntaxError
        text, tf = self.goto(self.loops[-1][1])
        self.append(text)
        if not tf:
            self.reachable = False

    def visitContinue(self, node):
        assert self.loops, "continue outside of loop" # SyntaxError
        text, tf = self.goto(self.loops[-1][0])
        self.append(text)
        if not tf:
            self.reachable = False

    
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
            if isinstance(node.expr, simple.SimpleListComp): 
                self.compileExpression(node.expr)
            else:    
                self.visit(node.expr)
        else:            
            for line in node.expr.args:
                assert isinstance(line, ast.Const), "can only INLINE strings"
                self.append(line.value)
            

    def visitPass(self, node):
        self.append("noop")

    def visitFunction(self, node):  # visitDef
        if self.classStack:
            fun = self.genFunction(node, node.name, allocate=0)
            self.locals[node.name] = fun
            klass = self.classStack[-1]
            self.append("setprop %s, '%s', %s" % (klass, node.name, fun))
        else:
            self.genFunction(node, node.name, allocate=1)
            

    def visitReturn(self, node):
        raise SyntaxError, "return outside of function"

    def visitYield(self, node):
        raise SyntaxError, "yield outside of function"
        
                
    ##[ exceptions ]####################################################

    def visitRaise(self, node):
        if not node.expr1:
            self.append("rethrow %s" % self.exception)
            return

        assert not (node.expr3), "3 arg raises not supported"
        self.append(self.set_lineno(node))

        regtext, exceptsym = self.gensym()
        if regtext is not None:
            self.append(regtext)        

        if node.expr2:
            type = self.compileExpression(node.expr1)
            obj  = self.compileExpression(node.expr2)
            self.append("%s = %s()" % (exceptsym, type))
            self.append("setref %s, %s" % (exceptsym,obj))
        else:
            msg = self.compileExpression(node.expr1)
            self.append("new %s, .Exception" % exceptsym)
            self.append('%s["_message"] = %s' % (exceptsym, msg))
            if msg == '"StopIteration"':
              self.append('%s["_type"] = 2' % exceptsym)

        self.append("throw %s" % exceptsym)

    def visitAssert(self, node):
        # another tree transformation -- if not .test: raise .fail
        self.visit( ast.If(
            tests = [(ast.Not(node.test),
                      ast.Raise(node.fail, None, None))],
            else_ = None ))


    def visitTryExcept(self, node):
        self.append(self.set_lineno(node))
        assert len(node.handlers)==1, "@TODO: only one handler for now"
        # assert not node.else_, "@TODO: try...else not implemented"
        catch = self.genlabel("catch")
        endtry = self.genlabel("endtry")
        self.append("push_eh " + catch)
        self.visit(node.body)
        if node.else_: self.visit(node.else_)
        self.append("clear_eh")
        text, tf = self.goto(endtry)
        self.append(text)
        if not tf:
            self.reachable = False
        selflinesappend, selfappend, indent = self.label(catch)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)        
        #self.goto(endtry)
        #self.label(catch)
        for hand in node.handlers:
            expr, target, body = hand
            prev_exception = self.exception
            regtext, self.exception = self.gensym()
            if regtext is not None:
                self.append(regtext)            
            self.append("%s = P5" % self.exception)
            if expr:
                expr = self.compileExpression(expr)
                expr = "%s.__match__(%s, %s)" % (expr, expr, self.exception)
                if target:
                    regtext, tmp = self.gensym()
                    if regtext is not None:
                        self.append(regtext)                    
                    self.append("%s = %s" % (tmp,expr))
                    self.assign(target,tmp)
                else:
                    self.append(expr)
            self.visit(body)
            self.exception = prev_exception
        selflinesappend, selfappend, indent = self.label(endtry, optional=True)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)        
        #self.label(endtry, optional=True)

                
        
    def visitTryFinally(self, node):
        # how do try/finally and continuations interact?
        # this is a hard question and should be brought up on the parrot list
        self.append(self.set_lineno(node))
        eh = self.genlabel("except")
        final = self.genlabel("final")
        endtry = self.genlabel("endtry")
        regtext, handler = self.gensym()
        if regtext is not None:
            self.append(regtext)
	# XXX: Does this work the same?
        #self.append("newsub %s, .Exception_Handler, %s" % (handler, eh))
        #self.append("set_eh " + handler)
	self.append("push_eh %s" % eh)
        self.visit(node.body)
        self.append("clear_eh")
        prev_exception = self.exception
        regtext, self.exception = self.gensym()
        if regtext is not None:
            self.append(regtext)        
        self.append("new %s, 'PyNone'" % self.exception)
        text, tf = self.goto(final)
        self.append(text)
        if not tf:
            self.reachable = False
        selflinesappend, selfappend, indent = self.label(eh)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)        
        #self.goto(final)
        #self.label(eh)
        self.append("%s = P5" % self.exception)
        selflinesappend, selfappend, indent = self.label(final)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)        
        #self.label(final)
        self.visit(node.final) 
        self.append(self.unless(self.exception, endtry))
        self.append("rethrow %s" % self.exception)
        selflinesappend, selfappend, indent = self.label(endtry)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)        
        #self.label(endtry)
        self.exception=prev_exception


    def visitClass(self, node):
        name = node.name
        regtext, klass = self.gensym()
        if regtext is not None:
            self.append(regtext)        
        
        if len(node.bases):
            assert len(node.bases) == 1, "Multiple bases not supported yet"
            super = self.compileExpression(node.bases[0])
        else:
            regtext, super = self.gensym()
            if regtext is not None:
                self.append(regtext)            
            self.append("getclass %s, 'PyType'" % super)

        self.append("subclass %s, %s, '%s'" % (klass, super, name))
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
    def __init__(self, name, depth, doc, args=[]):
        super(PirateSubVisitor, self).__init__(name)
        self.doc = doc
        self.args = args
        for (i,arg) in enumerate(self.args):
	    self.locals[arg] = arg
            #self.locals[arg] = "P%d" % (i+5)
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
        #print self
        code = self.lines
        self.lines = imclist()
        fallthru = self.reachable
        self.reachable = True

        if self.doc:
            self.append("")
            self.append("# %s" % self.doc, indent=False)
        
	# had :anon but took of when changed from set_addr to find_global to look
	# up functions
	self.append(".sub %s" % self.name, indent=False)
        #self.append("new_pad %s" % self.depth) #@TODO: use -1 here??
        for (i,arg) in enumerate(self.args):
	    # get parameters
            self.append(".param pmc %s" % arg)
	# do local bindings after params. params must be at top
	for(i,arg) in enumerate(self.args):
	    # bind them to local namespace
	    self.append(self.bindLocal(arg, arg))
	    #self.append(self.bindLocal(arg, "P%d" % (i+5)))

        self.lines.extend(code)

        if self.emptyReturn:
            fallthru = True
            selflinesappend, selfappend, indent = self.label(self.emptyReturn, optional=True)
            if selflinesappend is not None:
                self.lines.append(selflinesappend)
            if selfappend is not None:
                self.append(selfappend, indent)
                
#            self.label(self.emptyReturn, optional=True)

        if fallthru:
            regtext, dest = self.gensym()
            if regtext is not None:
                self.append(regtext)            
            self.append("new %s, %s" % (dest,self.find_type("PyNone")))
            self.append(".return (%s)" % dest)
        self.append(".end", indent=False)
        self.append("")
        return "\n".join(self.lines)

    def getCodeForGenerator(self):
        code = self.lines
        self.lines = imclist()
        fallthru = self.reachable
        self.reachable = True
        
        regtext, gen = self.gensym()
        if regtext is not None:
            self.append(regtext)
        regtext, result = self.gensym()
        if regtext is not None:
            self.append(regtext)

        name = self.name
        if self.doc:
            self.append("")
            self.append("# %s" % self.doc, indent=False)
        # had an :anon I removed it so could find by find_global
	self.append(".sub \"%s\"" % name, indent=False)
        for (i,arg) in enumerate(self.args):
            self.append(self.bindLocal(arg, "P%d" % (i+5)))

        label = self.genlabel("gen")
	#self.append("new %s, .Coroutine" % gen)
	#self.append("set_addr %s, %s" % (gen, label))
        self.append("newsub %s, .Coroutine, %s" % (gen,label))
        self.append("new %s, %s" % (result,self.find_type("PyGen")))
        self.append("setprop %s, 'next', %s" % (result,gen))

        self.append(".return (%s)" % result)
        selflinesappend, selfappend, indent = self.label(label)
        if selflinesappend is not None:
            self.lines.append(selflinesappend)
        if selfappend is not None:
            self.append(selfappend, indent)
#        self.label(label)
        #self.append("new_pad -1")

        self.lines.extend(code)

        if self.emptyReturn:
            fallthru = True
            selflinesappend, selfappend, indent = self.label(self.emptyReturn, optional=True)
            if selflinesappend is not None:
                self.lines.append(selflinesappend)
            if selfappend is not None:
                self.append(selfappend, indent)            
#            self.label(self.emptyReturn, optional=True)

        if fallthru:
            stop = ast.Raise(ast.Const("StopIteration"), None, None)
            self.visit(stop)

        self.append(".end", indent=False)
        return "\n".join(self.lines)
        

    def visitGlobal(self, node):
        for var in node.names:
            self.globals.append(var)

    def visitReturn(self, node):
        if isinstance(node.value,ast.Const) and node.value.value == None:
            self.emptyReturn = self.genlabel("return")
            text, tf = self.goto(self.emptyReturn)
            self.append(text)
            if not tf:
                self.reachable = False
#            self.goto(self.emptyReturn)
        else:
            result = self.compileExpression(node.value, allocate=1)
         #   self.append("pop_pad")
            self.append(".return (" + result + ")")
            self.reachable = False

    def visitYield(self, node):
        self.isGenerator = 1
        result = self.compileExpression(node.value, allocate=1)

        self.append(".yield (" + result + ")")
##########EMITTERS#############################
class PassEmitter(ast.Pass):
    def emit(self):
        yield "noop"        
        
class WhileEmitter(ast.While):
    def emit(self):
        a = PirateVisitor("__main__")
        yield beautify("hmm234932478723t4823g", 1)
        yield beautify("test")
        yield beautify("test2")
        #yield a.set_lineno(self)
        yield beautify("test3")        
        yield beautify(Dispatch(self.body).getLines())
        yield beautify("test4",2)        
        
def beautify(arg, val = 0):
    if val == 0:
        return "    " + str(arg) + "\n"
    elif val == 2:
        return "    " + str(arg)
    else:
        return str(arg) + "\n"

"""
        self.append(self.set_lineno(node))
        _while = self.genlabel("while")
        _elsewhile = self.genlabel("elsewhile") # sync nums even if no "else"
        _endwhile = self.genlabel("endwhile")
        self.loops.append((_while, _endwhile))
        
        self.label(_while)
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

"""
###############################################                
## module interface ###############################################
pir = PirateVisitor("__main__")
instanceList = []
initialRun = 1
    
import time
import transform
#import emitters
HEAD=\
"""
# generated by pirate on %s
.HLL 'Python', 'python_group'
""" % time.asctime()
FOOT=""

def replaceWith(newClass):
    def func(node):
        node.__class__ = newClass
        return node
    return func

def parse(src):
    return compiler.parse(src)
#SETH
def simplify(ast):
    t = transform.Transformer()
    t.on(compiler.ast.ListComp, simple.simplifyListComp)
    t.on(compiler.ast.Pass, replaceWith(PassEmitter))
    #t.on(compiler.ast.While, replaceWith(WhileEmitter))
    #t.on(compiler.ast.Discard, python.expressListComp)
    #t.on(compiler.ast.ListComp, python.expressListComp)
    #t.on(compiler.ast.Function, python.convertFunction)
    ast = t.apply(ast)
    return ast
                

def Dispatch(ast):
    vis = compiler.visitor.ASTVisitor()
    global initialRun
    if initialRun:
        initialRun = 0
        #pir = PirateVisitor("__main__")
        global pir
        vis.preorder(ast, pir)
        if isinstance(ast, compiler.ast.Module):
                pre  = ["    #loadlib P1, 'python_group'",
                        "    push_eh __py_catch"]
                post = ["    .return ()",
                        "__py_catch:",
			"    get_results '(0,0)', P0, S0",
                        "    print_item S0",
                        '    print_newline']
                pir.lines = pre + ["#"] + pir.lines + ["#"] + post
        return pir
    else:
        initialRun = 0
        inst = PirateVisitor("__main__")
        global instanceList
        instanceList.append(inst)
        vis.preorder(ast, inst)
        return inst
                    
def compile(ast, name="__main__"):
    return Dispatch(simplify(ast)).getCode()
    
def line_nos(seq):
    return [(i+1, seq[i]) for i in range(len(seq))]

def invoke(src, trace=0):
    i,o = os.popen4(trace and "parrot -t -" or "parrot -")
    tree = parse(src)
    tree = simplify(tree)
    code = compile(tree)
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
            print compile(compiler.parse(src))
            print FOOT
        else:
            for line in invoke(src, trace=("-t" in sys.argv)):
                sys.stdout.write(line)
    else:
        print __doc__
        sys.exit()
        

