import unittest
import pirate

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
    
def run(src, dump=0):
    return pirate.invoke(trim(src), dump)

class PirateTest(unittest.TestCase):

    def test_compile(self):
        # in general, won't actually test bytecode, but
        # this is just a simple case to show the interface
        res = pirate.compile(trim(
            """
            print
            """))
        goal = trim(
            """
            __main__:
                print "\\n"
                end
            """)
        assert res == goal, "bad bytecode from compile():\n%s" % res

    
    def test_print(self):
        res = run(
            """
            print 'hello,',
            print 'world!'
            """)
        self.assertEquals(res, "hello, world!\n")


    def test_if(self):
        res = run(
            """
            if 1:
                print 'shiver me timbers!'
                
            if 0:
                print 'ahoy maties!'
            else:
                print 'yar har har!'

            if 0:
                print 'walk the plank!'
            elif 1:
                print 'avast, ye landlubbers!'
            """)
        self.assertEquals(res,  "shiver me timbers!\n" \
                                 + "yar har har!\n" \
                                 + "avast, ye landlubbers!\n")

    def test_assignment(self):
        res = run(
            """
            a = 1
            b, c = 2, 3
            print a, b, c
            """)
        self.assertEquals(res, "1 2 3\n")


    def test_math(self):
        res = run(
            """
            x = (1 + 2) * 3 - 4
            print x % 3
            print 4/2  # note: this returns a float in parrot
            """)
        self.assertEquals(res, "2\n2.000000\n")

    def test_while(self):
        res = run(
            """
            x = 3
            while x:
                print x,
                x = x - 1
            """)
        self.assertEquals(res, "3 2 1 ")


    def test_compare(self):
        res = run(
            """
            #@TODO: print 1<2<3
            print 1==1, 0!=0, 1>0, 1<=5,
            print 1>=5, 4<>3
            """)
        self.assertEquals(res, "1 0 1 1 0 1\n")


    def test_euclid(self):
        # amk's example program:
        res = pirate.invoke(open("euclid.py").read(), dump=0)
        self.assertEquals(res, "96 64\n32\n")


if __name__=="__main__":
    unittest.main()
    
