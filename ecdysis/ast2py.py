"""
module ast2py -- turns python ast back into python code

usage:

    print ast2py.deparse(ast)

"""
import compiler.ast
import compiler.visitor
import cStringIO


def INDENT(s): s.indent += 1
def DEDENT(s): s.indent -= 1
def NEWLINE(s): s.newline()
def NEWBLOCK(s):
    s << ":"
    s << INDENT
    s << NEWLINE
def ENDBLOCK(s):
    s << DEDENT
    s << NEWLINE


HELP = object() # debugging aid

DEBUG = False

class IndentedStream(object):
    """
    An indented stream.

        >>> ind = IndentedStream()
        >>> ind << thing
        >>> ind.getvalue()

    where thing can be:

       - a string
       
       - INDENT
       - DEDENT
       - NEWLINE
       
       - NEWBLOCK
       - ENDBLOCK

       - any callable that takes an IndentedStream as its argument

    Note that indentation commands should come BEFORE the newline.
    This is a little counterintuitive, but it simplifies the code.
    """
    def __init__(self):
        self.result = cStringIO.StringIO()
        self.indent = 0
        self.oneLevel = "    "
        self.on_newline = True


    def newline(self):
        if self.on_newline: return
        self.result.write("\n")
        if self.indent: self.result.write(self.oneLevel * self.indent)
        self.on_newlite = True
        
    def __lshift__(self, thing):
        if callable(thing):
            thing(self)
        else:
            self.on_newline=False
            self.result.write(thing)

    def getvalue(self):
        return self.result.getvalue()


class Deparser(object):
    """
    This is the deparsing dispatch mechanism.

    Each onXYZ method corresponds to an AST node (defined in
    compiler.ast from the stdlib).

    These methods are generators yielding streams
    of tokens for an IndentedStream
    """

    def __init__(self, stream):
        self.stream = stream
        self.dispatch = {}
        for meth in dir(self):
            if meth.startswith("on"):
                self.dispatch[meth.replace("on", "visit")] = getattr(self,meth)

    def __getattr__(self, name):
        """
        The compiler.visitor module expects a bunch of
        .visitWhatever methods. But I want these methods
        to be generators, so I'm intercepting the call.
        I also change .visitXYZ to .onXYZ
        """
        if name in self.dispatch:
            return self.wrapVisit(self.dispatch[name])
        else:
            raise AttributeError(name)


    def wrapVisit(self, meth):
        """
        This allows us to use generators for the .visitNode methods.
        It would be a decorator, were it not for the .__getattr__ 
        """
        def wrapped(node, *args):
            for item in meth(node, *args):
                if item is None:
                    continue # for empty yield, e.g. onStmt
                if item is HELP:
                    self.stream << "#" * 50
                    self.dump(node)
                    self.stream << "#" * 50
                else:
                    self.stream << item
        return wrapped

    def walk(self, node):
        compiler.visitor.walk(node, self.__class__(self.stream))

    def dump(self, node):
        """
        this is for debugging: it adds a comment describing the
        ast node to the output stream
        """
        from inspect import formatargspec, getargspec
        self.stream << NEWLINE
        self.stream << "# spec = %s(%s)" % (
            node.__class__.__name__,
            formatargspec(getargspec(node.__class__.__init__)))
        self.stream << NEWLINE
        self.stream << "# node = %s" % str(node)
        self.stream << NEWLINE

    def makeTuple(self, tup, fmt="(%s)"):
        return fmt % ",".join([deparse(x) for x in tup])

    ## the node handlers ##########################################

    # Note for developers: yield HELP to dump the description of
    # the current node as a comment in the output stream.

    def onAssign(self, node):
        [self.walk(n) for n in node.nodes]
        yield " = "
        self.walk(node.expr)
        yield NEWLINE

    def onAssName(self, node):
        assert node.flags == "OP_ASSIGN"
        yield node.name


    def onCallFunc(self, node):
        self.walk(node.node)
        yield self.makeTuple(node.args)
        assert not node.star_args, "@TODO"
        assert not node.dstar_args, "@TODO"
        

    def onClass(self, node):
        yield "class %s" % node.name
        if node.bases:
            yield self.makeTuple(node.bases)
        yield NEWBLOCK
        self.walk(node.code)
        yield ENDBLOCK
        

    def onCompare(self, node):
        self.walk(node.expr)
        for op, expr in node.ops:
            yield op
            self.walk(expr)
        
    def onConst(self, node):
        yield repr(node.value)

    def onFunction(self, node):
        assert not node.decorators, "@TODO"
        yield "def %s" % node.name
        yield "(%s)" % ",".join(node.argnames)
        assert not node.defaults, "@TODO"
        yield NEWBLOCK
        self.walk(node.code, self.stream)
        yield ENDBLOCK


    def onGetattr(self, node):
        self.walk(node.expr)
        yield "."
        yield node.attrname

    def onIf(self, node):
        assert len(node.tests) == 1, "@TODO: elif"
        yield "if "
        test, then = node.tests[0]
        self.walk(test)
        yield NEWBLOCK
        self.walk(then)
        yield ENDBLOCK
        yield HELP

    def onImport(self, node):
        for real, mask in node.names:
            yield "import %s" % real
            if mask:
                yield " as %s" % mask
            yield NEWLINE

    def onMod(self, node):
        yield "("
        self.walk(node.left)
        yield " % "
        self.walk(node.right)
        yield ")"

    def onModule(self, node):
        if node.doc:
            yield node.doc
            yield NEWLINE
        self.walk(node.node)

    def onName(self, node):
        yield node.name


    def onPass(self, node):
        yield "pass"
        yield NEWLINE

    def onPrintnl(self, node):
        yield "print "
        yield self.makeTuple(node.nodes, "%s")
        yield NEWLINE

    def onReturn(self, node):
        yield "return "
        self.walk(node.value)

    def onStmt(self, node):
        for n in node.nodes:
            self.walk(n)
            yield NEWLINE

    def onSubscript(self, node):
        assert node.flags == "OP_APPLY", \
               "don't know how to handle %s" % node.flags 
        [self.walk(s) for s in node.subs]
        yield "["
        self.walk(node.expr)
        yield "]"
        

def visit(ast, visitorClass):
    stream = IndentedStream()
    visitorClass(stream).walk(ast)
    return stream.getvalue()
    


def deparse(ast):
    return visit(ast, Deparser)
