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
        self.counter = {}

    def symbol(self, prefix):
        """
        Return a unique symbol for label names in generated code
        """
        self.counter.setdefault(prefix,0)
        self.counter[prefix] += 1
        return "%s%05i" % (prefix, self.counter[prefix])
    

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

    logicOps = {
        compiler.ast.And: '&&',
        compiler.ast.Or: '||',
    }
    
    typeMap = {
        str: "PerlString",
        int: "PerlNum",
    }

    def expression(self, expr, dest):
        """
        create code to eval expression Node 'expr' and put it in 'dest'
        """
        if isinstance(expr, compiler.ast.Name):
            return ["%s = new PerlString" % (dest),
                    "%s = %s #XX" % (dest, expr.name)]
        elif isinstance(expr, compiler.ast.Const):
            t = type(expr.value)
            assert t in self.typeMap, "unsupported const type:%s" % t
            return [("%s = new %s" % (dest, self.typeMap[t])),
                    ("%s = %s" % (dest, repr(expr.value)))]
        
        elif isinstance(expr, tuple(self.infixOps.keys())):
            return self.infixExpression(expr, dest)
        elif isinstance(expr, compiler.ast.Compare):
            return self.compareExpression(expr, dest)
        else:
            print
            print
            print "*** UNKNOWN EXPRESSION ****"
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
        symleft,  typleft  = self.symbol("$P"), "PerlNum"
        symright, typright = self.symbol("$P"), "PerlNum"
        symexpr,  typexpr  = self.symbol("$P"), "PerlNum"
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
        res.append("%s = new PerlNum" % dest)
        res.append("if %s %s %s goto %s" % (symL, op, symR, _cmp))
        res.append("%s = 0" % dest)
        res.append("goto %s" % _end)
        res.append("%s:" % _cmp)
        res.append("%s = 1" % dest)
        res.append("%s:" % _end)
        return res


    ##[ visitor methods ]##########################################
        
    def visitPrint(self, node):
        assert node.dest is None, "print >> not yet handled"
        for n in node.nodes:
            self.extend(self.set_lineno(n))
            dest = self.symbol("$P")
            self.extend(self.expression(n, dest))
            self.append('print %s' % dest) # % repr(n.value))
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
            self.append("unless %s goto %s" % (test.value, _elif))
            
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
            self.append(".local string %s" % name)
            self.extend(self.expression(expr, name))


    def visitWhile(self, node):
        assert node.else_ is None, "while...else not supported"
        self.extend(self.set_lineno(node))
        _while = self.symbol("while")
        _endwhile = self.symbol("endwhile")
        self.append("%s:" % _while)
        testvar = self.symbol("$P")
        self.append("%s = new PerlInt" % testvar)
        self.extend(self.expression(node.test, testvar))
        self.append("unless %s goto %s" % (testvar, _endwhile))
        self.visit(node.body)
        self.append("goto %s" % _while)
        self.append("%s:" % _endwhile)


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

