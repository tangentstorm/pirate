"""parrot-gen.py

Parses a Python file and outputs a Parrot assembly file.

Currently this only understands a *really* limited subset of Python.
The only allowable type is integer; strings, floats, long ints, &c.,
aren't supported at all.  It will die with an assertion if you feed it
a language construct it doesn't handle (def, class, most operators).
"""

__revision__ = "$Id$"

import sys, os, types
import getopt, random

__doc__ = """%s: convert Python source to Parrot bytecode
  -r        Run Parrot assembler and interpreter on output
  -S        Only produce assembly source code (.pasm file)
  -t        Use trace option when running generated code
"""

# Location of the Parrot source tree
PARROT_SRC = "/home/amk/src/sf/parrot"

from compiler import ast, transformer, visitor, walk
from compiler.pycodegen import LocalNameFinder

# List 31 of the 32 PMC registers as being available
FREE_REGISTERS = []
for i in range(2, 32):
    FREE_REGISTERS.append('P' + str(i))

symcount = 1                            # Counter for symbols
def gensym ():
    """gensym() -> string
    Return a unique string that can be used as a label in the generated
    assembly code.
    """
    global symcount
    sym = 'sym%i' % symcount
    symcount += 1
    return sym
    

class ParrotVisitor:
    """Visitor class that outputs Parrot bytecode.

    Attributes:
      lines : [string]
        A list of strings, each one a line of assembly source code.
      functions : [string]
        More lines of assembly source, containing the functions.
    """
    
    def __init__ (self):
        self.lines = []
        self.functions = []
        self.symcount = 0               # Current number of symbol
        self.regcount = 0               # Current number of temp. register
        self._last_lineno = None

    def next_register (self, type='P'):
        reg = 'P%i' % self.regcount
        self.regcount += 1
        return reg
    
    def add_line (self, line):
        """add_line(line:string)
        Add a line of output to the generated code.  The line is
        modified slightly to ensure that it ends with a newline
        and is indented properly.
        """

        line = line.rstrip() + '\n'

        # Indent mnemonics
        # XXX this logic is obviously far too simple, but it'll do for now
        if ':' not in line:
            line = '\t' + line
        self.lines.append(line)

    def add_lines (self, lines):
        """add_lines(line:[string])
        Add several lines of output to the generated code.
        """
        for line in lines:
            self.add_line(line)

    def set_lineno (self, node):
        if (node.lineno is not None and
            node.lineno != self._last_lineno):
            self._last_lineno = node.lineno
            return ['setline %i' % node.lineno]
        else:
            return []

        
    # Visitor methods.  Most of them are currently unimplemented.
    def visitAssign (self, node):
        a_name = node.nodes[0].name
        reg = 'P0'#self.next_register()
        lines = (self.set_lineno(node) + 
                 compile_expr(node.expr, reg, FREE_REGISTERS) )
        self.add_lines(lines)
        self.add_line('store_global %s, "%s"' % (reg, a_name))
        # XXX assign result to 'a_name'

    def visitClass (self, node):
        assert 0, "class statement not supported"
    
    def visitIf (self, node):
        assert len(node.tests) == 1, "Only one test supported"
        test_expr, body = node.tests[0]
        sym = gensym()
        sym2 = gensym()

        lines = self.set_lineno(test_expr)
        lines += (compile_expr(test_expr, 'P0', FREE_REGISTERS) +
                 ["if P0, %s" % sym,
                  "branch %s" % sym2,
                  "%s:" % sym])
        self.add_lines(lines)
        self.visit(body)
        self.add_line("%s:" % sym2)
        
    def visitPrint (self, node):
        assert node.dest is None, "print without trailing newline not yet handled"

    def visitPrintnl (self, node):
        assert node.dest is None, "print>> not yet handled"
        lines = []
        for n in node.nodes:
            lines.extend(self.set_lineno(n))
            lines.extend(compile_expr(n, 'P0', FREE_REGISTERS))
            lines.extend(["print P0", 'print " "'])
        lines.append('print "\\n"')
        self.add_lines(lines)

##     def visitAssAttr(self, node):
##         assert 0, "not implemented"
##     def visitAssList(self, node):
##         assert 0, "not implemented"
##     def visitAssName(self, node):
##         assert 0, "not implemented"
##     def visitAssName(self, node):
##         assert 0, "not implemented"
##     def visitAssName(self, node):
##         assert 0, "not implemented"
##     def visitAssTuple(self, node):
##         assert 0, "not implemented"
    def visitAssert(self, node):
        assert 0, "not implemented"
##     def visitAugAssign(self, node):
##         assert 0, "not implemented"
##     def visitAugGetattr(self, node, mode):
##         assert 0, "not implemented"
##     def visitAugName(self, node, mode):
##         assert 0, "not implemented"
##     def visitAugSlice(self, node, mode):
##         assert 0, "not implemented"
##     def visitAugSubscript(self, node, mode):
##         assert 0, "not implemented"
##     def visitBackquote(self, node):
##         assert 0, "not implemented"
##     def visitBitxor(self, node):
##         assert 0, "not implemented"
    def visitBreak(self, node):
        assert 0, "not implemented"
    def visitCallFunc(self, node):
        assert 0, "not implemented"
    def visitClass(self, node):
        assert 0, "not implemented"
##     def visitCompare(self, node):
##         assert 0, "not implemented"
##     def visitContinue(self, node):
##         assert 0, "not implemented"
##     def visitDict(self, node):
##         assert 0, "not implemented"
##     def visitDict(self, node):
##         assert 0, "not implemented"
    def visitDiscard(self, node):
        lines = (self.set_lineno(node) +
                 compile_expr(node.expr, 'P0', FREE_REGISTERS) )
        self.add_lines(lines)
##     def visitEllipsis(self, node):
##         assert 0, "not implemented"
    def visitExec(self, node):
        assert 0, "not implemented"
    def visitFor(self, node):
        assert 0, "not implemented"
##     def visitFrom(self, node):
##         assert 0, "not implemented"
##     def visitFrom(self, node):
##         assert 0, "not implemented"
    def visitFunction(self, node):
        symbol = gensym()
        self.add_lines(['set_addr I0, %s' % symbol,
                        'new P0, .Sub',
                        'set P0, I0',
                        'store_global P0, "%s"' % node.name])
        
        # Generate code for the function body
        vtor = visitor.ASTVisitor()
        pv = ParrotVisitor()
        vtor.preorder(node.code, pv)
        self.functions += (['%s:' % symbol] + pv.lines + ['\t' 'ret'] +
                           pv.functions)

##     def visitGetattr(self, node):
##         assert 0, "not implemented"
##     def visitGlobal(self, node):
##         assert 0, "not implemented"
##     def visitGlobal(self, node):
##         assert 0, "not implemented"
##     def visitImport(self, node):
##         assert 0, "not implemented"
    def visitImport(self, node):
        assert 0, "not implemented"
##     def visitKeyword(self, node):
##         assert 0, "not implemented"
##     def visitLambda(self, node):
##         assert 0, "not implemented"
##     def visitLambda(self, node):
##         assert 0, "not implemented"
##     def visitList(self, node):
##         assert 0, "not implemented"
##     def visitListComp(self, node):
##         assert 0, "not implemented"
##     def visitListCompFor(self, node):
##         assert 0, "not implemented"
##     def visitListCompIf(self, node, branch):
##         assert 0, "not implemented"
##     def visitModule(self, node):
##         pass
##         #assert 0, "not implemented"
    def visitPass(self, node):
        self.add_lines(self.set_lineno(node))
        self.add_lines(["noop"])
##     def visitRaise(self, node):
##         assert 0, "not implemented"
    def visitReturn(self, node):
        lines = compile_expr(node.value, 'P0', FREE_REGISTERS[:])
        self.add_lines(lines)
        # XXX should ensure None is returned here
        self.add_line('\t' 'ret')
##     def visitSlice(self, node, aug_flag=None):
##         assert 0, "not implemented"
##     def visitSliceobj(self, node):
##         assert 0, "not implemented"
##     def visitSub(self, node):
##         assert 0, "not implemented"
##     def visitSubscript(self, node, aug_flag=None):
##         assert 0, "not implemented"
##     def visitTest(self, node, jump):
##         assert 0, "not implemented"
##     def visitTryExcept(self, node):
##         assert 0, "not implemented"
##     def visitTryFinally(self, node):
##         assert 0, "not implemented"
##     def visitTuple(self, node):
##         assert 0, "not implemented"

    def visitWhile(self, node):
        assert node.else_ is None, "while...else not supported"
        self.add_lines(self.set_lineno(node))
        start = gensym()
        end = gensym()

        self.add_line("%s:" % start)
        self.add_lines(self.set_lineno(node))
        self.add_lines(compile_expr(node.test, 'P0', FREE_REGISTERS))
        self.add_line('if P0, %s' % end)
        self.visit(node.body)
        self.add_line('branch %s' % start)
        self.add_line("%s:" % end)

def compile_expr (expr, dest_register, avail_registers):
    """compile_expr(expr:Node, dest_register:string,
                    avail_registers:[string]) -> [string]

    Generate bytecode to evaluate the resulting expression.  The
    result is left in the register named by 'dest_register';
    'avail_registers' is a list of unused registers.
    
    self.add_lines(self.set_lineno(expr))
    
    XXX This handles registers really stupidly, and always leaves its
    result in P0; it should allocate registers more efficiently.
    XXX how to handle string constants, or constants of other types?
    """
    dict_lr = {ast.Add: "add", ast.Sub: "sub",
            ast.Mul: "mul", ast.Div: "div",
            ast.Mod: "mod",
            ast.Power: "pow",
            ast.RightShift: 'shr', ast.LeftShift: 'shl'}
    dict_bool = {ast.And: 'and', ast.Or: 'or'}

    # XXX Parrot 0.0.7 seems to have a mysterious core-dumping bug
    # with register P1. 
    if dest_register == 'P1': raise RuntimeError
    
    # Copy avail_registers, because we'll be mutating it later
    avail_registers = avail_registers[:]
    lines = []

    # Ensure destination register isn't listed among available regs
    if dest_register in avail_registers:
        avail_registers.remove(dest_register)
    
    if isinstance(expr, ast.Const):
        t = type(expr.value) 
        assert t in [types.IntType, types.StringType,
                                    types.FloatType], \
                                    "Unsupported type: %r" % t
        if t is types.StringType:
            return ["new %s, .PerlString" % dest_register,
                    "set %s, %r" % (dest_register, expr.value)
                    ]
        else:
            # A number of some sort
            return ["new %s, .PerlNum" % dest_register,
                    "set %s, %r" % (dest_register, expr.value)
                    ]

    elif isinstance(expr, ast.Name):
        return ['find_global %s, "%s"' % (dest_register, expr.name)]
    
    elif isinstance(expr, tuple(dict_lr.keys())):
        opcode = dict_lr[expr.__class__]
        temp1 = random.choice(avail_registers)
        avail_registers.remove(temp1)
        temp2 = random.choice(avail_registers)
        avail_registers.remove(temp2)
        lines = (compile_expr(expr.left, temp1, avail_registers) +
                 compile_expr(expr.right, temp2, avail_registers) +
                 # perform operation
                 ["%s %s, %s, %s" % (opcode, dest_register,
                                     temp1, temp2)]
                )

    elif isinstance(expr, tuple(dict_bool.keys())):
        opcode = dict_bool[expr.__class__]
        temp1 = random.choice(avail_registers)
        avail_registers.remove(temp1)
        lines = []
        lines += compile_expr(expr.nodes[0], dest_register, avail_registers)
        for n in expr.nodes[1:-1]:
            lines += compile_expr(n, temp1, avail_registers)
            lines += ["%s %s, %s, %s" % (opcode, dest_register, dest_register, temp1)]
        lines += compile_expr(expr.nodes[-1], temp1, avail_registers)
        lines += ["%s %s, %s, %s" % (opcode, dest_register, dest_register, temp1)]
        

    elif isinstance(expr, ast.Compare):
        dict = {'<':'lt', '>':'gt', '==':'eq',
                '>=':'ge', '=>':'ge',
                '<=':'le', '=>':'le',
                '!=':'ne', '<>':'ne'}
        temp1 = random.choice(avail_registers)
        avail_registers.remove(temp1)
        temp2 = random.choice(avail_registers)
        avail_registers.remove(temp2)

        # XXX this code generation is doubtless wrong when there
        # are multiple comparison operators (as in 1<2<3).
        lines = compile_expr(expr.expr, temp1, avail_registers)
        for op, expr2 in expr.ops:
            lines += compile_expr(expr2, temp2, avail_registers)
            sym = 'true' + gensym()
            sym2 = 'done' + gensym()
            lines += ["%s %s, %s, %s" % (dict[op], temp1, temp2, sym),
                      "set %s, 0" % dest_register,      # False branch
                      "eq %s, %s, %s" % (dest_register, dest_register, sym2),
                      "%s:" % sym,
                      "set %s, 1" % dest_register,      # True branch
                      "%s:" % sym2]

    elif isinstance(expr, ast.CallFunc):
        assert expr.args == [], 'Arguments not handled'
        assert expr.star_args is None, '*args not handled'
        assert expr.dstar_args is None, '**args not handled'
        temp1 = random.choice(avail_registers)
        assert isinstance(expr.node, ast.Name), \
               "can only call functions directly"
        name = expr.node.name
        lines = compile_expr(expr.node, temp1, avail_registers)
        
        # XXX fix case where dest_register == 'P0'
        if dest_register != 'P0':
            lines.append('save P0')
        lines += ['find_global P0, "%s"' % name,
                  'call']
        if dest_register != 'P0':
            lines += ['add %s, P0, 0' % dest_register, 'restore P0']
    
    else:
        raise RuntimeError, "Unexpected node in expression: %r" % expr
    
    return lines



def generate_assembly (input_name, output_name):
    ast = transformer.parseFile(input_name)
##    print ast
    
    # Determine locals and assign them to registers
    # Disabled -- all variables are global for the moment
##    lnf = walk(ast, LocalNameFinder(), verbose=0)
##    for name in lnf.getLocals().elements():
##        reg = LOCAL_REGS[name] = random.choice(FREE_REGISTERS)
##        FREE_REGISTERS.remove(reg)
##    print LOCAL_REGS
    
    # Walk tree and generate bytecode
    vtor = visitor.ASTVisitor()
    pv = ParrotVisitor()

    for line in ["main:",
                 'setfile "%s"' % input_name,
                 'setpackage "__main__"']:
        pv.add_line(line)

    # Generate lines of assembly code
    vtor.preorder(ast, pv)
    pv.add_line('\t' 'end')
    
    # Write the generated assembly code
    lines = pv.lines + pv.functions
    output = open(output_name, 'w')
    output.writelines(lines)
    output.close()

    
def main():
    opts, args = getopt.getopt(sys.argv[1:], 'hrSt', ['help'])

    do_run = 0
    do_assemble = 1
    do_trace = 0
    for opt, param in opts:
        if opt in ['-h', '--help']:
            print __doc__ % sys.argv[0]
            sys.exit(0)
        elif opt == '-r':
            do_run = 1
        elif opt == '-S':
            do_assemble = 0
        elif opt == '-t':
            do_trace = 1
            
    if len(args) != 1:
        print __doc__ % sys.argv[0]
        sys.exit(0)
        
    for filename in args:
        root, ext = os.path.splitext(filename)
        asm_filename = root + '.pasm'
        bytecode_filename = root + '.pdbc'
        generate_assembly(filename, asm_filename)
        if do_assemble:
            err = os.system('perl -I%s %s/assemble.pl %s > %s'
                            % (PARROT_SRC, PARROT_SRC,
                               asm_filename, bytecode_filename) )
            if err: sys.exit(err)
        if do_run:
            if do_trace: trace_opt = '-t'
            else: trace_opt = ''
            cmd_line = '%s/parrot %s %s' % (PARROT_SRC, trace_opt,
                                            bytecode_filename)
            print cmd_line
            err = os.system(cmd_line)
            if err == 139:
                print 'Parrot interpreter dumped core'
            if err:
                sys.exit(err)
    
if __name__ == '__main__':
    main()

