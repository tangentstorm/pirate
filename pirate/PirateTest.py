import unittest
import pirate
import os.path

def trim(s):
    """
    strips leading indentation from a multi-line string.
    (for dealing with multi-line indented strings)
    """
    lines = s.split("\n")
    # strip leading blank line:
    if lines[0] == "":
        lines = lines[1:]
    # strip indendation:
    indent = len(lines[0]) - len(lines[0].lstrip())
    return "\n".join([line[indent:] for line in lines])


class PirateTest(unittest.TestCase):

    ## test support methods ###################

    def run(self, code, dump=0, lines=1):
        return pirate.invoke(trim(code), dump, lines=lines)

    def runTest(self, file):
        # determine the name of the baseline results
        outfile = os.path.splitext(file)[0] + ".out"

        # if baseline does not exist, run the test source through Python
        if not os.path.exists(outfile):
            i,o = os.popen4("python " + file)
            i.close()
            out = open(outfile,"w")
            out.write(o.read())
            out.close()
            o.close()

        # run the test and compare the results
        res = self.run(open(file).read()).strip()
        self.assertEquals(res, open(outfile).read().strip())


    ## simple vars/expressions ###################

    def test_unpack_wrong_size(self):
        try:
            res = self.run(
                """
                a,b = 1,2,3
                """, dump=0)
            gotError = 0
        except ValueError:
            gotError = 1
        assert gotError, "should get unpack sequence wrong size ValueError"


    def test_unpack_non_sequence(self):
        try:
            res = self.run(
                """
                a,b = 5
                """, dump=0)
            gotError = 0
        except TypeError:
            gotError = 1
        assert gotError, "should get unpack non-sequence TypeError"


    ## del #######################################

    def test_del_name(self):
        res = self.run(
            """
            x = 1
            del x
            print x
            """)
        self.assertEquals(res, "Lexical 'x' not found\n")

    def test_del_attr(self):
        res = self.run(
            """
            class C: pass
            C.x = 1
            print C.x
            del C.x
            print C.x
            """, dump=0)
        self.assertEquals(res, "1\nAttributeError: x\n")

    def test_del_key(self):
        res = self.run(
            """
            d = {'a':'b'}
            print d['a']
            del d['a']
            print d['a']
            """, dump=0)
        self.assertEquals(res, "b\nKeyError: a\n")


    ## amk's example program #####################

    def test_euclid(self):
        res = pirate.invoke(open("benchmarks/euclid.py").read(), dump=0)
        self.assertEquals(res, "96 64\n32\n")


    ## data structures ##############################

## @TODO: fix lambda/tuple syntax bug
##
##     def test_lambda_syntax_bug(self):
##         res = self.run(
##             ### the following SHOULD produce
##             ### a runtime error saying that
##             ### tuples are not callable.
##             ###
##             ### (because it's a 3-tuple):
##             ###
##             ###    lambda: 1
##             ###    2
##             ###    3
##             ###
##             ### but it doesn't. :)
##             """
##             a,b,c = (lambda: 1,2,3)()
##             """, dump=1, lines=1)


    ## exceptions ################################
    ## @TODO: top-level pythonic exception handler

    def test_raise(self):
        res = self.run(
            """
            print 'to be or ',
            raise 'not to be'
            print 'what was the question?'
            """, dump=0)
        # the error message shows up on stderr
        self.assertEquals(res, "to be or not to be\n")


    def test_try_finally(self):
        res = self.run(
            """
            try:
                print '1',
                raise hell
                print '2',
            finally:
                print '3',

            try:
                print '4',
            finally:
                print '5',
            """, dump=0)
        self.assertEquals(res, "1 3 4 5")

    def test_clear_eh(self):
        res = self.run(
            """
            try:
               pass
            except:
               print 'do not enter'
            raise hell
            """, dump=0)
        self.assertEquals(res, "Lexical 'hell' not found\n")


    ## object oriented stuff #####################

    ## @TODO: implement objects
    ## (waiting on python PMC's or parrot object layer)

if __name__=="__main__":
    import new
    import sys
    from glob import glob

    # For every test/*/filename.py file, create a test_filename method
    for test in sys.argv[1:] or glob("test/*/*.py"):
        testName = "test_" + os.path.splitext(os.path.split(test)[1])[0]

        testFunc = lambda self, test=test: self.runTest(test)
        testFunc.__doc__ = testName
        instanceMethod = new.instancemethod(testFunc, None, PirateTest)
        setattr(PirateTest, testName, instanceMethod)

    unittest.main(argv=sys.argv[:1])
