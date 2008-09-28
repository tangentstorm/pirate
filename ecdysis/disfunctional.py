

class DysFunctional(object):
    
    classData = {}
    one = 1

    def __init__(self):
        self.two = 2
        self.classData["three"] = 3

    def count(self):
        four = 4
        five = 4
        five += 1
        
        print self.one
        print self.two
        print self.classData["three"]
        print four
        print five
   
    def whatif(self, test):
        if test:
            print "yep"
        else:
            print "nope"

    def whatfor(self, data):
        result = []
        for x in data:
            if x % 2: continue
            if x == 9: break
            result.append(x)
        else:
            print "nothing to see here"
        return result

    def dowhile(self):
        x = 1
        while (x < 65536):
            x *= 2
            print x
        else:
            print "can't a guy get a break?"


    def exceptional(self):
        try:
            raise Exception("oh no")
        except Exception, e:
            print "okay"
        finally:
            print "that wasn't so exceptional."

       
    def whatswrong(self):
        assert self.exceptional() is None
        assert self.whatif(True) is None
        assert self.whatif(False) is None
        assert self.whatfor(range(10)) == [0,2,4,6,8]
        assert self.whatfor(range(2)) == [0]
        assert self.dowhile() is None

d = DysFunctional()
d.whatswrong()



# transcendental python: episode 1: a functional imperative.

# http://docs.python.org/ref/ sec 5 and 6
# expression statements, assign , augassign,
# assert, pass, del, print, return, yield, raise, break,
# continue import (__future__),  global, exec

# everything else is an expression:
# it either returns a value or throws an exception
# expressions cango on the right side of an = sign.


#   - expressive (functional, object-oriented/message passing)
#         - function calls
#         - lookups
#         - lambda
#   - imperative (dysfunctional)
#        - look and feel
#        - readability
#        - but restrains us
# 
# slowly becoming more functional over time
# as of 2.5:
#    - list comps (for, if)
#    - immediate if
#    - yield as expression




# * class is series of assignments in a scope
# * function defs are assignments
# * print / append -> yield or return
# * whatif : inline if
# * whatelse : nested block! uhoh.

"""

A big part of what makes functional programs easier to read is
that they group similar operations rather than similar nouns.

Similar data structures is sort of the point of object oriented
programimng. What's different is the methods.

It's fine to group those things together on the UML diagram,
but when it comes to the code it makes more sense to group
by the verb, because that's where you're most likely to
see the duplication.

you're much more likely to duplicate ideas when you're doing
the same thing, but far away in the code. 

"""
