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

    ## simple vars/expressions ###################
    
    def test_print(self):
        res = self.run(
            """
            print 'hello,',
            print 'world!'
            """)
        self.assertEquals(res, "hello, world!\n")


    def test_assignment(self):
        res = self.run(
            """
            a = 1
            b, c = 2, 3
            [d,e] = 'f', 'g'
            print a, b, c, d, e
            """)
        self.assertEquals(res, "1 2 3 f g\n")


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
            
    def test_unpack_list(self):
        res = self.run(
            """
            a = []
            print a,
            b = [1]
            print b,
            c = [1,2,3]
            print c,
            """, dump=0)
        self.assertEquals(res, "[] [1] [1, 2, 3] ")
            

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
            print x % 3,
            print 4/2  # @TODO: this returns a float in parrot
            """, dump=0)
        self.assertEquals(res, "2 2.000000\n")


    def test_compare(self):
        res = self.run(
            """
            #@TODO: print 1<2<3
            print 1==1, 0!=0, 1>0, 1<=5,
            print 1>=5, 4<>3,
            """)
        self.assertEquals(res, "1 0 1 1 0 1 ")


    def test_logic(self):
        res = self.run(
            """
            print 'cat' or 'mouse',
            if not (1 and 0): print 'and',
            print 'cat' and 'mouse',
            """, dump=0)
        self.assertEquals(res, "cat and mouse ")


    def test_bitwise(self):
        res = self.run(
            """
            print 1 & 3,     # == 1
            print 1 & 2 & 3, # == 0
            print 1 | 3,     # == 3
            print 1 | 2 | 3, # == 3
            print 1 ^ 3,     # == 2
            print 1 ^ 2 ^ 3, # == 0
            print (15 & 22) | (1 ^ 9)  # == 14
            """, dump=0)
        self.assertEquals(res, "1 0 3 3 2 0 14\n")


    def test_shift(self):
        res = self.run(
            """
            print 1 << 2,
            print 1 << 2 << 2,
            print 1 >> 2,
            print 256 >> 2,
            print 256 >> 2 >> 2,
            """, dump=0)
        self.assertEquals(res, "4 16 0 64 16 ")

            
    def test_unary(self):
        res = self.run(
            """
            x = 1
            print -x,
            print +x,
            print ~x,
            print not x, 
            """, dump=0, lines=1)
        self.assertEquals(res, "-1 1 -2 0 ")


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
            print C.x,
            del C.x
            print C.x
            """, dump=0)
        self.assertEquals(res, "1 AttributeError: x\n")

    def test_del_key(self):
        res = self.run(
            """
            d = {'a':'b'}
            print d['a'],
            del d['a']
            print d['a']
            """, dump=0)
        self.assertEquals(res, "b KeyError: a\n")


    ## pass ######################################
        
    def test_pass(self):
        res = self.run(
            """
            pass
            """, dump=0, lines=0)
        self.assertEquals(res, "")


    ## control structures ########################

    def test_while(self):
        res = self.run(
            """
            x = 3
            while x:
                print x,
                x = x - 1
            """)
        self.assertEquals(res, "3 2 1 ")


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



    ## amk's example program #####################

    def test_euclid(self):
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


    def test_dict(self):
        res = self.run(
            """
            print {}, {'a':'b'},
            d = {'x':'c'}
            d['y'] = 'z'
            print d['x'], d['y']
            """, dump=0, lines=0)
        self.assertEquals(res, "{} {'a': 'b'} c z\n")
            
            
    ## function-related stuff ####################

    def test_minimal_function(self):
        res = self.run(
            """
            def f(): return 1
            print f()
            """, dump=0, lines=1)
        self.assertEquals(res, "1\n")
            

    def test_arg_order(self):
        res = self.run(
            """
            def dump(a, b):
                print a, b, 
            dump('a','b')
            """, dump=0, lines=0)
        self.assertEquals(res, "a b ")


    def test_lambda(self):
        res = self.run(
            """
            f = lambda x: x+1
            print f(4), f(5)
            """, dump=0, lines=1)
        self.assertEquals(res, "5 6\n")

        
    def test_lambda_anonymous(self):
        res = self.run(
            """
            print (lambda x: x*x)(5)
            """, dump=0, lines=0)
        self.assertEquals(res, "25\n")

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


    def test_return_tuple(self):
        res = self.run(
            """
            a, b, c = (lambda: (1,2,3))()
            print a, b, c
            a = (lambda: (4,5,6))()
            print a
            """, dump=0, lines=1)
        self.assertEquals(res, "1 2 3\n[4, 5, 6]\n")


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


    def test_no_return(self):
        res = self.run(
            """
            def f(): pass
            print f()
            """, dump=0,lines=0)
        self.assertEquals(res, "None\n")


    ## scope tests ###############################
        
    def test_lexical_scope(self):
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


    def test_global(self):
        res = self.run(
            """
            def change():
                global x
                x = 'dog'
            x = 'cat'
            change()
            print x
            """, dump=0)
        self.assertEquals(res, "dog\n")


    ## list comprehensions #######################

    def test_simple_listcomp(self):
        res = self.run(
            """
            print [x for x in ['c','a','t'] if x !='c']
            """, dump=0)
        self.assertEquals(res, "['a', 't']\n")


    def test_listcomp(self):
        res = self.run(
            """
            print [x+y  for x in (1,2,3,4,5,6,7,8)
                        if x > 4
                        for y in (10,20)
                        if x < 7]
            """, dump=0)
        self.assertEquals(res, "[15, 25, 16, 26]\n")


    ## exceptions ################################
    ## @TODO: top-level pythonic exception handler
    
    def test_raise(self):
        res = self.run(
            """
            print 'to be or',
            raise 'not to be'
            print 'what was the question?'
            """, dump=0)
        # the error message shows up on stderr
        self.assertEquals(res, "to be or not to be\n")
            
    def test_assert(self):
        res = self.run(
            """
            assert 1, 'oh cool'
            assert 0, 'aw crap'
            print 'do not enter'
            """, dump=0)
        self.assertEquals(res, "aw crap\n")

    def test_try_except(self):
        res = self.run(
            """
            try:
                print 'parrot',
                raise hell
                print 'dropped the ball'
            except:
                print 'caught it!'
            """, dump=0)
        self.assertEquals(res, "parrot caught it!\n")

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
        self.assertEquals(res, "1 3 4 5 ")

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


    def test_except_called_raise(self):
        res = self.run(
            """
            def raise_exception():
               raise hell
            try:
               raise_exception()
               print 'fumbled.'
            except:
               print 'caught it!'
            """, dump=0)
        self.assertEquals(res, "caught it!\n")


    ## generators and iterators ##################

    def test_generator(self):
        res = self.run(
            """
            from __future__ import generators #@TODO: 2.3
            def count():
                yield 0
                yield 1
            gen = count()
            #@TODO: catch StopIteration (uncatchable due to parrot bug)
            if 1:
            #try:
                print gen.next(),
                print gen.next(),
            #   print gen.next(),
            #except:
            #    print 'done'
            """, dump=0, lines=0)            
        self.assertEquals(res, "0 1 ") #done\n")

    def test_microthreads(self):
        res = self.run(            
            """
            from __future__ import generators #@TODO: 2.3
            def make_count(step):
                def count():
                    x = 0
                    while 1:                        
                        yield step
                        print x
                        print step + x
                        #print x+ step
                        #x = x + step
                        #x = x + step
                return count()
            gs = [make_count(0), make_count(1)]
            for g in gs:
                print g.next(),
            """, dump=1, lines=0)
        self.assertEquals(res, "0 0 0 1 0 2 0 3")
            
## @TODO: implement iterators (for x in g)
## waiting on *.next() for all objects.
## otherwise, we have to write special case code
##
##     def test_iterator(self): 
##         pass


    ## object oriented stuff #####################

    ## @TODO: implement objects
    ## (waiting on python PMC's or parrot object layer)

    def test_class(self):
        res = self.run(
            """
            class Stuff:
                x = 'x'
            print Stuff.__name__ #@TODO:, Stuff.x
            """, dump=0)
        self.assertEquals(res, "Stuff\n")

    def test_instance(self):
        res = self.run(
            """
            class Thing:
                pass
            x = Thing #@TODO: Thing()
            x.y = 1
            print x.y
            """, dump=0)
        self.assertEquals(res, "1\n")


    ## PARROT_INLINE ##################################

    def test_PARROT_INLINE(self):
        res = self.run(
            """
            PARROT_INLINE(
                r'S0 = "cat\\n"',
                r'print S0'
            )
            """, dump=0)
        self.assertEquals(res, 'cat\n')


if __name__=="__main__":
    unittest.main()
