"""
ecdysis: python sheds its skin
"""
# license appears at bottom

import re
import inspect
import compiler
import unittest
import narrative as narr

"""
use : as lambda? eg:

(: (a b c) (+ a b c))

It's odd, but : already defines a block in python.

what about the control structures?
    - while
    - if
    - for
    - try
    - raise
    - break
    - continue


"""



# * an abstract syntax for python
"""
<p>In nature, ecdysis is the process a reptile goes
through when it sheds its skin. Here, it refers to a
version of the python programming language stripped
of its syntax.</p>

<p>Yes, it looks like lisp, and it even borrows a few
tricks from the lisp family, but don't be fooled:
ecdysis is python all the way down.</p>

<p>So why on earth would you want to use it?</p>

<ul>
<li><strong>brevity</strong></li>
<li><strong>macros</strong</li>
<li><strong>metaprogramming</strong></li>
</ul>
"""



# * chains : linked lists of cons cells
# ** what the heck are cons cells?
"""
<p>One of the main data structures from the lisp world is
the cons cell: essentially a pair of objects. Cons cells
can be chained together to make linked lists.</p>

<p>Python has a very nice built in list type, but sometimes
a lisp-style list can be more useful. For example, programs
written in a functional style tend to break lists into heads
and tails, and this is much faster with cons cells (because
in the ptyhon version, the assignment to <code>tail</code>
reates a completely new copy of the list)</p>

   >>> head, tail = pylist[0] , pylist[1:] # inefficient
   >>> head, tail = cons_cell              # much faster

<p>The cons cell is the first major concept that we're
adding to python, and it works like this:</p>
"""

@narr.testcase
def test_cons_pair(self):
    cell = cons(1, 2)
    h, t = cell.pair()
    assert h == 1
    assert t == 2

"""
<p>Cons cells are mutable, so they're basically like a
two-element python lists. We'll define <code>cons</code>
as a subtype of <code>list</code>.</p>
"""
class cons(list):
    @narr.snapshot
    def __init__(self, head, tail):
        self.extend([head, tail])
    def pair(self):
        return self[0], self[1]

# ** two items only
"""
However, we should make sure that they only have 2 values.
"""

@narr.testcase
def test_cons_size(test):
    cell = cons("a", "b")
    test.assertRaises(NotImplementedError, cell.extend, [None])
    test.assertRaises(NotImplementedError, cell.append, None)

"""
Easy enough:
"""
@narr.addMethod(cons)
def extend(self, data):
    raise NotImplementedError

@narr.addMethod(cons)
def append(self, data):
    raise NotImplementedError

"""
But that means we need super() to get at the initial call:
"""
@narr.replaceMethod(cons)
def __init__(self, head, tail):
    super(cons, self).extend([head, tail])

# ** linking behavior
"""
<p>To create a linked list, you the tail of the cons cell
should point to another cons cell. The last item in the
chain is a special value, 'nil'.</p>
"""
@narr.testcase
def test_linked_list(test):
    chain = cons("a", cons("b", cons("c", nil)))
    assert list(chain) == ["a", "b", "c"]

"""
<p>To do this, we just need to do is define <code>__iter__</code>
to walk the structure recursively:</p>
"""
@narr.addMethod(cons)
def __iter__(self):
    yield self[0]
    tail = self[1]
    if tail is nil:
        pass
    elif isinstance(tail, cons):
        for item in self[1]:
            yield item
    else: # improper or "dotted" pair
        yield tail

"""
<p>For now, <code>nil</code> is just an arbitrary value.</p>
"""
class nil:
    pass

#@narr.replaceMethod(cons)
#def __iter__(self):
        
    


# ** chains are represented as s-expressions
"""
<p>The lisp-like syntax allows you express these structures much
more concisely. We'll create a parser to build cons cells from
text in a moment, but let's work on displaying the data:</p>
"""
@narr.testcase
def test_cons_str(test):
    data = cons(1, cons(2, cons(3, cons("a", cons("b", cons("c", nil))))))
    test.assertEquals(str(data),  "(1 2 3 'a' 'b' 'c')")
    test.assertEquals(str(data), repr(data))

"""
<p>And of course we just use <code>__str__</code>:</p>
"""
@narr.addMethod(cons)
def __str__(self):
    return "(%s)" % " ".join([repr(x) for x in self])

@narr.addMethod(cons)
def __repr__(self):
    return str(self)



# * a few chain-related functions
"""
<p>Before we can implement the parser, we need a few
general-purpose helper routines.</p>
"""

def head(cell):
    return cell[0]

def tail(cell):
    return cell[1]


def reverse(el):
    """
    reverse an ecdysis list
    """
    # implementation taken from:
    # http://www.cs.sfu.ca/CC/310/pwfong/Lisp/2/tutorial2.html
    def _rev_helper(el, a):
        if el is nil:
            return a
        return _rev_helper(tail(el), cons(head(el), a))
    return _rev_helper(el, nil)



# * parsing s-expressions
# ** test case
"""
<p>:</p>
"""
@narr.testcase
def test_read(test):
    goal = {
        "'a'"       : 'a',
        "1"         : 1,
        "()"        : nil,
        "(1)"       : [1, nil],
        "(1 2)"     : [1, [2, nil]],
        "(1 2 3)"   : [1, [2, [3, nil]]],
        "(() ())"   : [nil, [nil, nil]],
        "(1 () 2)"  : [1, [nil, [2, nil]]],
        "(1 (2) 3)" : [1, [[2, nil], [3, nil]]],
    }
    for s in goal.keys():
        test.assertEquals(read(s), goal[s])


"""
<p>The system is divided into two parts: the parser
   and the lexer.</p>
"""
# ** the lexer
"""
a lexer turns a plain string into a series of tokens

the idea for this lexer idea was stolen from:
http://jason.diamond.name/weblog/2005/04/26/lexical-analysis-python-style

see also:
http://effbot.org/zone/xml-scanner.htm
"""

SPACE    = "SPACE"
LPAREN   = "LPAREN"
RPAREN   = "RPAREN"
INTEGER  = "INTEGER"
FLOAT    = "FLOAT"
VARNAME  = "VARNAME"
OPERATOR = "OPERATOR"
PLUS     = "PLUS"

lexer = re.compile(r'''
    (?P<%(SPACE)s>      \s+              )
|   (?P<%(LPAREN)s>     \(               )
|   (?P<%(RPAREN)s>     \)               )
|   (?P<%(INTEGER)s>    \d+              )
|   (?P<%(FLOAT)s>      \d*\.\d+         )
|   (?P<%(VARNAME)s>    [_a-zA-Z]\w*     )
|   (?P<%(PLUS)s>       \+               )
|   (?P<%(OPERATOR)s>   [-*/]            )
''' % (locals()), re.VERBOSE)


class Token(object):
    def __init__(self, type, value):
        self.type = type
        if INTEGER == type:
            self.value = long(value)
        elif FLOAT == type:
            self.value = float(value)
        else:
            self.value = value
            
def tokenize(source):
    """
    this is a generator that yields tokens
    given an ecdysis source code string
    """
    # we walk through the regular expression
    # match objects and yield Tokens for each one.
    for t in lexer.finditer(source):
        # t.lastgroup is the ?P<..> label from the regexp
        token = Token(t.lastgroup, t.group(t.lastgroup))
        if token.type == SPACE:
            continue # ignore whitespace
        yield token
    # yields StopIteration rather than throwing it
    # so we can play functional programmer :)
    yield Token(StopIteration, None)


# ** the parser : read()

"""
<p>a parser takes the stream of tokens created by
the lexer and builds it into a syntax tree</p>

<p>the <a href='http://www.cs.utexas.edu/users/wilson/schintro/schintro_115.html#SEC137'>algorithm for this parser</a> was taken from:
<em>An Introduction to Scheme and its Implementation</em>
(an online book by Paul R Wilson)</p>
"""

def read(sexp):
    """
    parse the s-expression and return
    the corresponding data structure
    """
    data = _read_next(tokenize(sexp))
    if isinstance(data, Token):
        return data.value
    else:
        return data


def _read_next(stream):
    token = stream.next()
    if LPAREN == token.type:
        # start a new list:
        return _read_list(stream, nil)
    else:
        return token

def _read_list(stream, list_so_far):
    # Turns out it's simpler to do this by adding
    # values to the front of list_so_far, so we
    # have to reverse when we hit the right paren
    next = _read_next(stream)
    if RPAREN == next.type:
        return reverse(list_so_far)
    elif LPAREN == next.type:
        return _read_list(stream,
                         cons(_read_list(stream, nil),
                              list_so_far))
    else:
        if isinstance(next, Token):
            next = next.value
        return _read_list(stream, cons(next, list_so_far))


nil.type = nil
cons.type = cons




# compiling the code

# * a prototyping the evaluator
# ** first step: simple math expressions
"""
<p>Let's start simple here with a prototype
solution, and see what we can come up with.</p>
"""
@narr.testcase
def test_plus(self):
    assert ecds_proto("(+ 1 0)") == 1
    assert ecds_proto("(+ 1 1)") == 2
    assert ecds_proto("(+ 1 2)") == 3
    assert ecds_proto("(+ 2 3)") == 5

"""
<p>Well, the simplest thing we could do is:</p>
"""
def ecds_proto(code):
    f, a, b = read(code)
    return a + b

"""
<p>That works for plus, but what about the other operators?</p>
"""
@narr.testcase
def test_simple_math(self):
    assert ecds_proto("(- 1 2)") == -1
    assert ecds_proto("(* 1 0)") == 0
    assert ecds_proto("(+ 1 0)") == 1
    assert ecds_proto("(/ 4 2)") == 2

"""
We could use the <code>operator</code> module here, but
we could also do this:
"""

@narr.replace
def ecds_proto(code):
    f, a, b = read(code)
    return eval("%s %s %s" % (a, f, b))
# ** simple function calls
"""
It's tempting to jump directly for something like (+ 1 2 3 4 5)
now, but that's a lisp idea. For now, in python, + is purely
binary.

How about range()?
"""

@narr.testcase
def test_function_call(self):
    assert ecds_proto("(range 5)") == [0, 1, 2, 3, 4]
    assert ecds_proto("(range 4 8)") == [4, 5, 6, 7]
    assert ecds_proto("(range 0 10 2)") == [0, 2, 4, 6, 8]

"""
<p>Well, we'd need to distinguish beetween binary operators
and function calls.</p>
"""

OPERATORS = "+ - in / // & ^ | ** is << % * >> < <= == != >= >".split()

@narr.replace
def ecds_proto(code):
    head, tail = read(code).pair()
    if head in OPERATORS:
        a, b = tail
        return eval("%s %s %s" % (a, head, b))
    else:
        return eval(head)(*list(tail))

"""
<p>Our evaluator prototype is already starting to
get out of hand. It makes sense to back up and
think through exactly how we want to do this.</p>
"""


# * execution contexts
"""
<p>Ideally, we would be able to mix code freely
between python and ecdysis.</p>

<p>As long as we can compile the ecdysis source
into a python <a href='http://docs.python.org/ref/types.html#l2h-138'
>code object</a>, we can use it with the <code>eval</code>
function or the <code>exec</code> statement</p>

"""

# * black magic : ec_exec and ec_eval
"""
<p>Ideally, we would be able to create wrapper
functions that invoked <code>exec</code>
or <code>eval</code> from inside the caller's
environment. We can use some black magic to
make this happen, but unfortunately, there
are a few restrictions.</p>

<p>It would work like this:</p>
"""
@narr.testcase
def test_execute_up(test):
    data = {"key":"value"}
    execute_up('data["key"]="changed"', levels_up=1)
    assert data["key"]=="changed"

"""
<p>And it turns out you can do this sort of thing -
<em>almost</em>. The following function grabs the
<code>local</code> and <code>global</code> dictionaries
for a frame higher up on python's call stack:</p>
"""
def execute_up(code, levels_up=2):    
    frame = inspect.stack()[levels_up][0]
    exec code in frame.f_locals, frame.f_globals

"""
<p>Unfortunately, while you can modify objects inside
the calling scope with this technique, you can't always
reassign variables. It works at the top level:</p>
"""
if __debug__:
    value = 'original'
    execute_up("value = 'updated'", levels_up=1)
    assert value == 'updated' , value

"""
<p>But unfortunately, assignments break when this
technique is used inside a function (or test case):</p>
"""
@narr.testcase
def test_exec_up(test):
    value = 'original'    
    execute_up("value = 'updated'", levels_up=1)
    assert value == 'original' # Sadly :(

"""
<p>So, if you want to use these functions, you can,
but only from within 
"""


# * a refined evaluator


def ecds(source):
    tree = read(source)      # produces a tree of cons cells
    return ec_compile(tree)  # build exec-utable code object



def ec_compile(source):
    if head in OPERATORS:
        a, b = tail
        return eval("%s %s %s" % (a, head, b))
    else:
        return eval(head)(*list(tail))


"""
<p>Our code here is already starting to look like
a dispatch method. What we need to do is look at
the first element of the chain, and then map that
to a function.</p>

<p>We also want to separate the process of reading
from the process of evaluating. Ideally, we would
read the code into a cons tree, allow for any
preprocessing or macros, and then convert each node
in the tree into a <code>Node</code> object suitable
for python's <code>compiler.ast</code> module.</p>
"""




# * major eval test
# ** the end goal
"""
<p>Now for the fun stuff. The s-expressions are essentially
a textual mapping of python's abstract syntax tree. It still
<em>behaves</em> (mostly) like python - it just looks funny.</p>

<p>If we were to write scheme in python or lisp in python, we'd
have a whole mess of work to do here. But this is just python,
and python already has a powerful compiler package.</p>

<p>Once we parse the s-expressions into a tree of cons cells,
the next step is to convert the tree into a real python abstract
syntaxt tree, as supported by the <code>compiler</code>
module. (We could attempt to regenerate actual python source
code, but part of the reason for using lisp syntax is to
allow expressions that are hard to express in python)</p>

<p>So. Is it possible to map symbols to ast nodes automatically?
Let's write a test case.</p>

<p>Our goal will be to produce a function, <code>ecdys</code>
that compiles and executes a piece of ecdysis code. You may notice
the odd string delimiters ("''' '"  and "' '''"). They're ugly,
but they trick emacs python-mode into syntax-coloring the code,
so that's just how it's going to be. :) If you prefer, you can
just use normal quotes.</p>
"""

@narr.testcase
def test_ecds_proto(test):
    ecds_proto(
    ''' '
    
    # here's what ecdysis code looks like:
    (class MyClass (object)
     """
     example ecdysis class.
     """
     (def __init__ (self x y)
      # use "set=" instead of "=" for clarity
      (set= self.x x)
      (set= self.y y))
     
     (def foo (self a)
      # implicit 'return' statement:
      (+ (a (* self.x self.y)))))

    (set= obj (MyClass 10 20))
    (assert (== (obj.foo 3) 203
             "MyClass(10,20).foo(3) should evaluate to 203"))
    ' ''')

    # and the code should be "live" in the calling context:
    assert issubclass(MyClass, object)
    assert isinstance(obj, MyClass)
    assert obj.foo(5) == 205


# * notes
if False:
    ec_exec(
    '''#'
    (class Whatever (unittest.TestCase)
     (def test (self)
      (assert (== 5 5) "math done broke!")))
    
    (def f (a b c)
     (+ a b c))
    
    (assert (== 6 (f 1 2 3)))
    
    :symbols
    (assert (== "b" (getitem { "a" "b" } "a")))
    
    (map (lambda (a b) (+ a b)) [ 1 2 3 4 5 ])
    
    (for a in (range 10)
     (sys.stdout.write (+ a "\n")))
    
    (macro-def snoopydoop (a b c)
     ^(a b ,c))
    ''')#')

# * @TODO
"""
 - negative numbers in the reader
"""
# * self-test
if __name__=="__main__":
    unittest.main()
    

# * license
"""
Copyright (c) 2005, Michal J Wallace
Some portions Copyright (c) 2003, Miles Egan
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * The name of the author may not be used to endorse or promote
      products derived from this software without specific prior
      written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""
