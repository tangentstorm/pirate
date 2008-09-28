
from pyparsing import *


def start(char, name):
     return Literal(char).setParseAction(lambda *a: name)

# comment
comment = start("#", "comment") + restOfLine

# xml
xmlId = Word(alphas,alphanums+"_-")
tag = start("<", "tag") + xmlId + Suppress(">")
end = start("</", "end") + xmlId + Suppress(">")
empty = start("<", "empty") + xmlId + Suppress("/>")
xml = tag | end | empty

# operators
ADD = start("+", "ADD")
SUB = start("-", "SUB")
MUL = start("*", "MUL")
DIV = start("/", "DIV")
MOD = start("%", "MOD")
BITAND = start("&", "BITAND")
BITOR  = start("|", "BITOR")
BITXOR = start("^", "BITXOR")
operator = ADD | SUB | MUL | DIV | MOD | BITAND | BITOR | BITXOR

# literal
symbol = Empty().setParseAction(lambda: "sym") + Word(alphas)
integer = Empty().setParseAction(lambda: "int") + Word(nums).setParseAction(lambda s,l,toks: int(toks[0]))
number = integer
literal = Group(symbol | number)

# lisp
null = start("(", "null") + Suppress(")")
def_ = start("def", "def")
keyword = null | def_
form = start("(", "form") + OneOrMore(operator | keyword | literal ) + Suppress(")")
chain = form | null

lisp = symbol | chain
parser = comment | xml | lisp 

####################################################################################3
#
# demo stuff ( for reference )
# lbrack = Suppress("[")
# rbrack = Suppress("]")
#
# listStr = Forward()
#integer = Word(nums)
#integer.setName("integer")
#integer.setParseAction(lambda s,l,toks: int(toks[0]))
#
#
#real = Combine(Optional(oneOf("+ -")) + Word(nums) + "." + Optional(Word(nums)))
#real.setName("real")
#real.setParseAction(lambda s,l,toks: float(toks[0]))
#
#listItem = real | integer | quotedString.setParseAction(removeQuotes) | Group(listStr)
#
#listStr << ( lbrack + delimitedList(listItem) + rbrack 
#
#test = "['a', 100, 3.14, [ +2.718, 'xyzzy', -1.414] ]"
#print listStr.parseString(test)
#
####################################################################################3


import unittest

tests = [
    ("#",       ["comment", ""]),
    ("a",       ["sym", "a"]),
    ("()",      ["null"]),
    ("(+ a b)", ["form", "ADD", ["sym", "a"], ["sym", "b"]]),
    ("(+ 1 2)", ["form", "ADD", ["int", 1], ["int", 2]]),
    ("(+ a b)", ["form", "ADD", ["sym", "a"], ["sym", "b"]]),
    ("(a b c)", ["form", ["sym", "a"], ["sym", "b"], ["sym", "c"]]),
    ("<html>",  ["tag", "html"]),
    ("</html>", ["end", "html"]),
    ("<data/>", ["empty", "data"]),

    # from the scarlet Lambda Site:
    ('(def main)' , ["form", "def", ["sym", "main"]]),
    
 #   ('''(def main ()
 #           ^"Hell yeah.")''', ["def", "main", "null", ["^", ["str", "hello world"]]])
]

####################################################################################3
class GoalTest(unittest.TestCase):
    def test(self):
        for num, (text, goal) in enumerate(tests):
            try:
                actual = parser.parseString(text)
                self.assertEqual(str(goal), str(actual))
            except Exception, e:
                self.fail("%s raised Exception: %s" % (text, e)) 
if __name__ == '__main__':
    unittest.main()

