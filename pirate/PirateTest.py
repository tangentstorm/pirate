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

    
    def test_print(self):
        result = pirate.invoke(trim(
            """
            print 'hello,',
            print 'world!'
            """), dump=0)
        self.assertEquals(result, "hello, world!\n")


    def test_if(self):
        result = pirate.invoke(trim(
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
            """), dump=0)
        self.assertEquals(result,  "shiver me timbers!\n" \
                                 + "yar har har!\n" \
                                 + "avast, ye landlubbers!\n")

    def test_assignment(self):
        result = pirate.invoke(trim(
            """
            a = 1
            b, c = 2, 3
            print a, b, c
            """), dump=0)
        self.assertEquals(result, "1 2 3\n")


    def test_math(self):
        result = pirate.invoke(trim(
            """
            x = (1 + 2) * 3 - 4
            print x % 3
            print 4/2  # note: this returns a float in parrot
            """), dump=0)
        self.assertEquals(result, "2\n2.000000\n")

    def test_while(self):
        result = pirate.invoke(trim(
            """
            x = 3
            while x:
                print x,
                x = x - 1
            """), dump=0)
        self.assertEquals(result, "3 2 1 ")



if __name__=="__main__":
    unittest.main()
    
