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

    def run(self, code, dump=0, lines=1):
        return pirate.invoke(trim(code), dump, lines=lines)

    
    def test_print(self):
        res = self.run(
            """
            print 'hello,',
            print 'world!'
            """)
        self.assertEquals(res, "hello, world!\n")


    def test_if(self):
        res = self.run(
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
        res = self.run(
            """
            n = 1
            if n > 0:
                print 'x'
            """, dump=0)
        self.assertEquals(res, "x\n")
        

    def test_assignment(self):
        res = self.run(
            """
            a = 1
            b, c = 2, 3
            [d,e] = 'f', 'g'
            print a, b, c, d, e
            """)
        self.assertEquals(res, "1 2 3 f g\n")


    def test_simplemath(self):
        res = self.run(
            """
            print 1+2+3
            """, dump=0)
        self.assertEquals(res, "6\n")

    def test_math(self):
        res = self.run(
            """
            x = (1 + 2) * 3 - 4
            print x % 3
            print 4/2  # note: this returns a float in parrot
            """, dump=0)
        self.assertEquals(res, "2\n2.000000\n")


    def test_while(self):
        res = self.run(
            """
            x = 3
            while x:
                print x,
                x = x - 1
            """)
        self.assertEquals(res, "3 2 1 ")


    def test_compare(self):
        res = self.run(
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


    ## data structures ##############################

    def test_list(self):
        res = self.run(
            """
            print [], [1], [1,2,3], [[],[4],[]]
            """)
        self.assertEquals(res, "[] [1] [1, 2, 3] [[], [4], []]\n")

    def test_tuple(self):
        #@TODO: make tuples be tuples once there's a .PythonTuple
        res = self.run(
            """
            print (1,2,(3,4),5)            
            """)
        self.assertEquals(res, "[1, 2, [3, 4], 5]\n")
        
    
    def test_for(self):
        res = self.run(
            """
            for num in [1, 2, 3, 4, 5]:
                print num * num,
            print
            """, dump=0)
        self.assertEquals(res, "1 4 9 16 25 \n")

    def test_break(self):
        res = self.run(
            """
            for x in [1, 2]:
                for y in [1,2,3,4,5]:
                    if y >1: break
                    print [x,y],
            
            num = 0
            while 1:
                num = num + 1
                print num,
                if num == 2: break
            """)
        self.assertEquals(res, "[1, 1] [2, 1] 1 2 ")

    def test_continue(self):
        res = self.run(
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
        res = self.run(
            """
            print 'cat' or 'mouse',
            if not (1 and 0): print 'and',
            print 'cat' and 'mouse',
            """, dump=0)
        self.assertEquals(res, "cat and mouse ")


## @TODO: re=enable this with set-lex
##     def test_function(self):
##         res = self.run(
##             """
##             __py__print('function call!') # from pirate.imc!!
##             """, dump=0)
##         self.assertEquals(res, "function call!")

        
    def test_pass(self):
        res = self.run(
            """
            pass
            """, dump=0, lines=0)
        self.assertEquals(res, "")


    ## function-related stuff ##############################

    def test_minimal_function(self):
        res = self.run(
            """
            def f(): return 1
            print f()
            """, dump=0, lines=1)
        self.assertEquals(res, "1\n")
            

    def test_lambda(self):
        res = self.run(
            """
            f = lambda x: x+1
            print f(4), f(5)
            """, dump=0, lines=0)
        self.assertEquals(res, "5 6\n")

        
    def test_lambda_anonymous(self):
        res = self.run(
            """
            print (lambda x: x*x)(5)
            """, dump=0, lines=0)
        self.assertEquals(res, "25\n")


    def test_def(self):
        res = self.run(
            """
            def f():
                return 1
            print f()
            """, dump=0, lines=1)
        self.assertEquals(res, "1\n")


    def test_multi_return(self):
        res = self.run(
            """
            def f(x):
                if x:
                    return 1
                else:
                    return 0
            print f(1), f(0)
            """, dump=0, lines=0)
        self.assertEquals(res, "1 0\n")


    def test_nested_call(self):
        res = self.run(
            """
            def g():
                return 0
            def f():
                return g()
            print f()
            """, dump=0, lines=0)
        self.assertEquals(res, "0\n")


    def test_recursion(self):
        res = self.run(
            """
            def f(x):
                print x,
                if x==0:
                    return 0
                else:
                    return f(x-1)
            f(1)
            """, dump=0, lines=0)
        self.assertEquals(res, "1 0 ")

    def test_scope(self):
        res = self.run(
            """
            x = 1
            def make_adder(base):
                def adder(x):
                    return base+x
                return adder
            h = make_adder(10)
            print h(5), x
            """, dump=0, lines=0)
        self.assertEquals(res, "15 1\n")


if __name__=="__main__":
    unittest.main()
