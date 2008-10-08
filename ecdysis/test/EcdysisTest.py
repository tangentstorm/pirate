import unittest
from ecdysis import eeval

class EcdysisTest(unittest.TestCase):

    def test_ops(self):
        self.assertEquals(1, eeval("1"))
        self.assertEquals(2, eeval("(+ 1 1)"))
        self.assertEquals(0, eeval("(- 1 1)"))
        self.assertEquals(6, eeval("(* 2 3)"))


    def test_etrans(self):
        #f = eeval("(lambda (a b) (+ a b))")
        #self.assertEquals(2, f(1,2))

if __name__=="__main__":
    unittest.main()
    
