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

def run(src, dump=0, lines=1):    
    return pirate.invoke(trim(src), dump, lines=0)

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
            .sub __main__
                print "\\n"
                end
            .end
            """)
        assert res.startswith(goal), "bad bytecode from compile():\n%s" % res

    
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
            """, dump=0)
        self.assertEquals(res,  "shiver me timbers!\n" \
                                 + "yar har har!\n" \
                                 + "avast, ye landlubbers!\n")
        
    def test_if_expr(self):
        res = run(
            """
            n = 1
            if n > 0:
                print 'x'
            """, dump=0)
        self.assertEquals(res, "x\n")
        

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
            print 1>=5, 4<>3,
            """)
        self.assertEquals(res, "1 0 1 1 0 1 ")


    def test_euclid(self):
        # amk's example program:
        res = pirate.invoke(open("euclid.py").read(), dump=0)
        self.assertEquals(res, "96 64\n32\n")


    def test_list(self):
        res = run(
            """
            print [], [1], [1,2,3], [[],[4],[]]
            """)
        self.assertEquals(res, "[] [1] [1, 2, 3] [[], [4], []]\n")
    
    def test_for(self):
        res = run(
            """
            for num in [1, 2, 3, 4, 5]:
                print num * num,
            print
            """, dump=0)
        self.assertEquals(res, "1 4 9 16 25 \n")

    def test_break(self):
        res = run(
            """
            for num in [1, 2, 3, 4, 5]:
                print num, 
                if num == 2: break
            
            num = 0
            while 1:
                num = num + 1
                print num,
                if num == 2: break
            """)
        self.assertEquals(res, "1 2 1 2 ")

    def test_continue(self):
        res = run(
            """
            for num in [1, 2, 3, 4, 5]:
                if num < 4: continue
                print num,
            
            num = 0
            while num<5:
                num = num + 1
                if num < 4: continue
                print num,
            """, dump=0)
        self.assertEquals(res, "4 5 4 5 ")


    def test_logic(self):
        res = run(
            """
            print 'cat' or 'mouse',
            if not (1 and 0): print 'and',
            print 'cat' and 'mouse',
            """, dump=0)
        self.assertEquals(res, "cat and mouse ")


    def test_function(self):
        res = run(
            """
            __py__print('function call!') # from pirate.imc!!
            """, dump=0)
        self.assertEquals(res, "function call!")

        
    def test_lambda(self):
        res = run(
            """
            f = lambda x: x+1
            print f(4), f(5)
            """, dump=0, lines=0)
        self.assertEquals(res, "5 6\n")
        
    def test_scope(self):
        res = run(
            """
            x = 5
            f = lambda: x+1
            print f()
            x = x + 1
            print f()
            """, dump=1)
        self.assertEquals(res, "6 7\n")
        
        
if __name__=="__main__":
    unittest.main()
    
