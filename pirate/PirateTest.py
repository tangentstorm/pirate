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
    

class PirateTest(unittest.TestCase):


    def test_compile(self):
        # in general, won't actually test bytecode, but
        # this is just a simple case to show the interface
        result = pirate.compile(trim(
            """
            print
            """))
        goal = trim(
            """
            __main__:
                print "\\n"
                end
            """)
        assert result == goal, "bad bytecode from compile():\n%s" % result

    
    def testPrint(self):
        result = pirate.invoke(trim(
            """
            print 'hello, world!'
            """), dump=0)
        self.assertEquals(result, "hello, world!\n")


    def testIf(self):
        result = pirate.invoke(trim(
            """
            if 1:
                print 'shiver me timbers!'
                
            if 0:
                print 'ahoy maties!'
            else:
                print 'yar har har!'
            """), dump=0)
        self.assertEquals(result, "shiver me timbers!\nyar har har!\n")


if __name__=="__main__":
    unittest.main()
    
