
import unittest
import PirateTest

class PPythonTest(PirateTest.PirateTest):
    """
    http://www.cathoderaymission.net/~logistix/python/pparser.py

    To attach to the compiler package, save this file in
    Lib/compiler and change the 'import parser' statement to
    'import compiler.pparser as parser'.
    """

    def setUp(self):
        import pparser
        import compiler
        compiler.transformer.parser = pparser

    


if __name__=="__main__":
    unittest.main()
    
