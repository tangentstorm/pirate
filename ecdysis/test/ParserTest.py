import unittest
from ecdysis import read, null, write, cons, reverse

## some s-expressions and their
## corresponding pythonic representations:
sexps = {
    "a"         : "a",
    "1"         : 1,
    "()"        : null,
    "(1)"       : [1, null],
    "(1 2)"     : [1, [2, null]],
    "(1 2 3)"   : [1, [2, [3, null]]],
    "(() ())"   : [null, [null, null]],
    "(1 () 2)"  : [1, [null, [2, null]]],
    "(1 (2) 3)" : [1, [[2, null], [3, null]]],
}

class ParserTest(unittest.TestCase):

    def test_read(self):
        for s in sexps:
            self.assertEquals(read(s), sexps[s])

    def test_write(self):
        for s in sexps:
            self.assertEquals(s, write(read(s)))

    def test_reverse(self):
        self.assertEquals(reverse(read("(1 2 3)")),
                          read("(3 2 1)"))

if __name__=="__main__":
    unittest.main()
    
