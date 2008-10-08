"""
ecdysis parser module
"""
import re
from scheme import *
from handy.switch import switch

# * tokens 

# * lexer 
"""
a lexer turns a plain string into a series of tokens

the idea for this lexer idea was stolen from:
http://jason.diamond.name/weblog/2005/04/26/lexical-analysis-python-style

see also:
http://effbot.org/zone/xml-scanner.htm
"""
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
        for case in switch(type):
            if case(INTEGER):
                self.value = long(value)
            elif case(FLOAT):
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



## helper functions:





# * read()
        
