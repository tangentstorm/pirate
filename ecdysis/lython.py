#!/usr/bin/env python
# -*- python -*-
# $Id$

import compiler
from compiler import ast
import dis
import getopt
from spark import GenericScanner, GenericASTBuilder, GenericASTTraversal, GenericParser
import string
import sys
import types
import ast2py

def flattenable(o): return type(o) == list

def flatten(x, b = None):
    if b is None: b = []
    for i in x:
        if flattenable(i):
            flatten(i, b)
        else:
            b.append(i)
    return b
        
def foldtree(node):
    if hasattr(n, 'attr'):
        return n
    else:
        return flatten(map(foldtree, n._kids))

def showtree(node, depth=0):
    if node.attr:
        print "%2d" % depth, " " * depth * 2, ".", node.type, " <",
        print node.attr,
        print "> "
    else:
        print "%2d" % depth, "-" * depth * 2, ".", node.type
        for n in node._kids:
            showtree(n, depth + 1)

class AST:
    def __init__(self, type, attr = None, kids = None):
        self.type = type
        self.attr = attr
        self._kids = kids or []
    def __getitem__(self, i):
        return self._kids[i]
    def __len__(self):
        return len(self._kids)
    def __setitem__(self, i, j):
        self._kids[i] = j
    def __setslice__(self, low, high, seq):
        self._kids[low:high] = seq
    def __cmp__(self, o):
        return cmp(self.type, o)
    def __repr__(self):
        return "<AST %s : %s >" % (self.type, self.attr)

class MyCodeGenerator(compiler.pycodegen.InteractiveCodeGenerator):
    pass
    
class Token:
    def __init__(self, type, attr=None):
        self.type = type
        self.attr = attr

    def __cmp__(self, o):
        return cmp(self.type, o)

    def __repr__(self):
        return self.type

class LythonScanner(GenericScanner):
    def __init__(self):
        GenericScanner.__init__(self)
    
    def tokenize(self, input):
        self.rv = []
        GenericScanner.tokenize(self, input)
        return self.rv
    
    def t_whitespace(self, s):
        r' [ \t\n]+ '
        pass

    def t_comma_id(self, s):
        r'\,[_a-zA-Z][_a-zA-Z0-9]*(\.[_a-zA-Z0-9]+)*'
        self.rv.append(Token(type="comma_id", attr=s))


    def t_comment(self, s):
        r' \#[^\n]+ '
        pass

    def t_float(self, s):
        r'-? \d+ \. \d+ '
        t = Token(type='float', attr=s)
        self.rv.append(t)
        
    def t_operator(self, s):
        r' \+ | \* | - | := | = | if | >= | > | <= | < | != | % | @ | : '
        self.rv.append(Token(type='operator', attr=s))

    def t_parenl(self, s):
        r'\('
        self.rv.append(Token(type=s))
        
    def t_parenr(self, s):
        r' \)'
        self.rv.append(Token(type=s))

    def t_attr(self, s):
        r'\.[_a-zA-Z][_a-zA-Z0-9]*'
        self.rv.append(Token(type='attr', attr=s))

    def t_id(self, s):
        r'[_a-zA-Z][_a-zA-Z0-9]*(\.[_a-zA-Z0-9]+)*'
        self.rv.append(Token(type = 'id', attr = s))
        
    def t_number(self, s):
        r'-? \d+ '
        t = Token(type='number', attr=s)
        self.rv.append(t)

    def t_string(self, s):
        r'"[^"]+"'
        self.rv.append(Token(type='string', attr=s[1:][:-1]))

class LythonParser(GenericASTBuilder):
    def p_expr(self, args):
        '''
        expr ::= number
        expr ::= float
        expr ::= operator
        expr ::= list
        expr ::= comma_id
        expr ::= id
        expr ::= string
        expr ::= attr
        expr ::= expr expr
        list ::= ( )
        list ::= ( expr )
        '''

    def terminal(self, token):
        rv = AST(token.type, token.attr)
        return rv

    def nonterminal(self, type, args):
        if type == 'expr' and len(args) > 1:
            # flatten expression trees
            rv = self.AST('expr', None, args[0]._kids + args[1]._kids)
            return rv
        elif type == 'list':
            # flatten list expressions
            return self.AST('list', None, args[1]._kids)
        elif type == 'operator':
            return self.AST('operator', args[0].type)
        else:
            rv = self.AST(type)
            rv[:len(args)] = args
            return rv

class LythonCompiler(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        self.macros = {}

    def pn(self, node):
        self.preorder(node)
        return node.value

    def compile(self):
        self.preorder()
        self.ast.value = ast.Module(
            None,
            ast.Stmt(self.ast.value))
        self.ast.value.filename = '<lython>'
        return self.ast

    def n_expr(self, node):
        node.value = [self.pn(x) for x in node._kids]

    def n_float(self, node):
        node.value = ast.Const(float(node.attr))

    def n_id(self, node):
        node.value = node.attr
        parts = string.split(node.attr, ".")
        if len(parts) == 1:
            if node.attr == 'false':
                node.value = ast.Const(False)
            elif node.attr == 'true':
                node.value = ast.Const(True)
            else:
                node.value = ast.Name(node.attr)
        else:
            node.value = ast.Getattr(ast.Name(parts[0]), parts[1])
            for part in parts[2:]:
                node.value = ast.Getattr(node.value, part)

    def n_list(self, node):
        oper = None
        # check for empty list
        if len(node._kids) == 0:
            node.value = None
            return
        else:
            oper = node._kids[0]


        # macro definition
        if oper.type == 'id' and oper.attr == 'defmacro':
            expansion = node._kids[3:]
            self.macros[node._kids[1].attr] = Macro(node._kids[1].attr, node._kids[2], expansion)
            node.value = ast.Pass()
        elif oper.type == 'id' and oper.attr == 'pass':
            # naw pass is just an expression that evals to None
            node.value = ast.Name('None') 
        # functions
        elif oper.type == 'id' and oper.attr == 'def':
            node.value = ast.Function(
                None, #@TODO: decorators
                node._kids[1].attr,
                [x.attr for x in node._kids[2]._kids],
                [],
                0,
                None,
                ast.Stmt([self.pn(x) for x in node._kids[3:-1]]
                         + [ast.Return(self.pn(node._kids[-1]))]))
            node.value.filename = '<lython>' # XXX why is this necessary?
        # print
        elif oper.type == 'id' and oper.attr == 'print':
            node.value = ast.CallFunc(
                ast.Name('lispprint'),
                [self.pn(node._kids[1])],
                None,
                None)
            
        # import
        elif oper.type == 'id' and oper.attr == 'import':
            node.value = ast.Import([(x.attr, None) for x in node._kids[1:]])
        elif oper.type == 'id' and oper.attr == 'class':
            node.value = ast.Class(node._kids[1].attr,
                                   [self.pn(x) for x in node._kids[2]],
                                   node._kids[3].attr,
                                   ast.Stmt([self.pn(x) for x in node._kids[4:]]),
                                   '<lython>')
            node.value.filename = "<lython>"
            
        # list
        elif oper.type == 'id' and oper.attr == 'list':
            node.value = ast.List([self.pn(x) for x in node._kids[1:]])
        # and/or
        elif oper.type == 'id' and oper.attr == 'and':
            node.value = ast.And([self.pn(x) for x in node._kids[1:]])
        elif oper.type == 'id' and oper.attr == 'or':
            node.value = ast.Or([self.pn(x) for x in node._kids[1:]])
        # for loop
        elif oper.type == 'id' and oper.attr == 'for':
            if len(node._kids) == 4:
                # for x form
                node.value = ast.For(
                    ast.AssName(node._kids[1].attr, 'OP_ASSIGN'),
                    self.pn(node._kids[2]),
                    self.pn(node._kids[3]),
                    None)
            else:
                # for x y ... form
                node.value = ast.For(
                    ast.AssTuple([ast.AssName(x.attr, 'OP_ASSIGN') for x in node._kids[1:-2]]),
                    self.pn(node._kids[-2]),
                    self.pn(node._kids[-1]),
                    None)

        # while loop
        elif oper.type == 'id' and oper.attr == 'while':
            node.value = ast.While(
                self.pn(node._kids[1]),
                ast.Stmt([self.pn(x) for x in node._kids[2:]]),
                None)

        # compound statements (block)
        elif oper.attr == ':':
            node.value = ast.Stmt([(self.pn(x))
                                   for x in node._kids[1:]])


        # getattr
        elif oper.attr.startswith("."):
            node.value = ast.Getattr(self.pn(node._kids[1]),
                                     oper.attr[1:])

        # index
        elif oper.attr == '@':
            if len(node._kids) == 3:
                node.value = ast.Subscript(self.pn(node._kids[1]), 'OP_APPLY', [self.pn(node._kids[2])])
            else:
                node.value = ast.Slice(self.pn(node._kids[1]), 'OP_APPLY', self.pn(node._kids[2]), self.pn(node._kids[3]))
        # plus
        elif oper.attr == '+':
            node.value = ast.CallFunc(
                ast.Name('lispplus'),
                [self.pn(x) for x in node._kids[1:]],
                None,
                None)
        # minus
        elif oper.attr == '-':
            node.value = ast.CallFunc(
                ast.Name('lispminus'),
                [self.pn(x) for x in node._kids[1:]],
                None,
                None)
        # times
        elif oper.attr == '*':
            node.value = ast.CallFunc(
                ast.Name('lisptimes'),
                [self.pn(x) for x in node._kids[1:]],
                None,
                None)
        # mod
        elif oper.attr == '%':
            if len(node._kids) == 3:
                node.value = ast.Mod([self.pn(x) for x in node._kids[1:]])
            else:
                node.value = ast.Mod(
                    (self.pn(node._kids[1]), ast.Tuple([self.pn(x) for x in node._kids[2:]])))
        # assignment
        elif oper.attr == ':=':
            node.value = ast.Assign(
                [ast.AssName(node._kids[1].attr, 'OP_ASSIGN')],
                self.pn(node._kids[2]))
        # equality
        elif oper.attr == '=':
            node.value = ast.Compare(
                self.pn(node._kids[1]),
                [('==', self.pn(node._kids[2]))])
        # other comparison
        elif oper.attr in [ '<', '<=', '>', '>=', '!=' ]:
            node.value = ast.Compare(
                self.pn(node._kids[1]),
                [(oper.attr, self.pn(node._kids[2]))])
        # conditional
        elif oper.attr == 'if':
            if len(node._kids) == 3:
                node.value = ast.If(
                    [(self.pn(node._kids[1]), self.pn(node._kids[2]))], None)
            else:
                node.value = ast.If(
                    [(self.pn(node._kids[1]), self.pn(node._kids[2]))], self.pn(node._kids[3]))
        # evaluate a macro
        elif self.macros.has_key(oper.attr):
            macro = self.macros[oper.attr]
            if len(node._kids[1:]) != macro.arity:
                raise "syntax error - arity of macro %s is %s" % (macro.name, macro.arity)
            node.value = ast.Stmt([self.pn(x) for x in macro.expand(node._kids[1:])])
        # call a function
        else:
            node.value = ast.CallFunc(
                self.pn(oper),
                [self.pn(x) for x in node._kids[1:]],
                None,
                None)

    def n_number(self, node):
        node.value = ast.Const(int(node.attr))
        
    def n_operator(self, node):
        node.value = node

    def n_string(self, node):
        node.value = ast.Const(node.attr)

    def n_comma_id(self, node):
        node.value = node

    def n_attr(self, node):
        node.value = node

   
    def default(self, node):
        raise "missed a case:" ,node

class Macro(object):
    def __init__(self, name, args, expansion):
        self.name = name
        self.args = [x.attr for x in args._kids]
        self.expansion = expansion

    def _get_arity(self):
        return len(self.args)

    def _expand(self, env, expr):
        expand = None
        if expr.type == 'comma_id':
            expand = env[expr.attr[1:]]
        elif expr.type == 'list':
            expand = AST("list", None)
            expand._kids = [self._expand(env, x) for x in expr._kids]
        else:
            expand = expr
        return expand
    
    def expand(self, args):
        env = {}
        for arg,val in zip(self.args,args):
            env[arg] = val
        exp = [self._expand(env, x) for x in self.expansion]
        return exp

    arity = property(_get_arity)

def lispminus(*args):
    return reduce(lambda x,y: x - y, args)
            
def lispplus(*args):
    return reduce(lambda x,y: x + y, args)
            
def lisptimes(*args):
    return reduce(lambda x,y: x * y, args)

def lispprint(*args):
    print " ".join(args)

lython_env = {
    'lispplus': lispplus,
    'lisptimes' : lisptimes,
    'lispminus' : lispminus,
    'lispprint' : lispprint,
    'Macro' : Macro,
    '__name__' : '__main__',
}

def usage():
    print "usage: lython scriptfile"
    sys.exit(1)




if __name__ == '__main__':
    DEBUG = 0
    opts, args = getopt.getopt(sys.argv[1:], "d")
    for o, a in opts:
        if o == '-d':
            DEBUG = 1
            #args.remove("-d")
            break
    sys.argv = args
    if len(args) < 1:
        usage()
    filename = sys.argv[0]
    f = open(filename)
    input = f.read()
    f.close()
    
    
    tokens = LythonScanner().tokenize(input)
    
    tree = LythonParser(AST, 'expr').parse(tokens)
    if DEBUG:
        print tree.type
        showtree(tree)
        print

    codeast = LythonCompiler(tree).compile()
    if DEBUG:
        print codeast.value
        print
        print "---------"


    ast2py.DEBUG = DEBUG 
    py =  ast2py.deparse(codeast.value)

    if DEBUG:
        print py + "# end"

    exec (py, lython_env)
    
    #code = compiler.pycodegen.ModuleCodeGenerator(codeast.value).getCode()
    #if DEBUG:
    #    print dis.dis(code)
    #exec (code, lython_env)
