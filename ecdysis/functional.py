
from primitives import *


x_block(*apply(lambda x_scope = globals() : [
lambda : x_class(x_scope, "Functional", (object,), {

  "classData" : {},
  "one" : 1,


  "__init__" : lambda self :
    x_block(*apply(lambda x_locals=locals() : [
      (lambda : setattr(self, "two", 2)),
      (lambda : x_assign(self.classData, "three", 3))])),


  "count" : lambda self :
    x_block( *apply(lambda x_locals=locals() : [
      (lambda : x_assign(x_locals, "four", 4)),
      (lambda : x_assign(x_locals, "five", 4)),
      (lambda : x_assign(x_locals, "five", x_locals["five"] + 1)),
      (lambda : x_print(self.one)),
      (lambda : x_print(self.two)),
      (lambda : x_print(self.classData["three"])),
      (lambda : x_print(x_locals["four"])),
      (lambda : x_print(x_locals["five"]))])),


  "dowhile" : lambda self :
    x_block(*apply(lambda x_locals=locals() : [
      (lambda : x_assign(x_locals, "x", 1)),
      (lambda : x_while((lambda : x_locals["x"] < 65536),
            (lambda : x_block(
              (lambda : x_assign(x_locals, "x", x_locals["x"] * 2)),
              (lambda : x_print(x_locals["x"]))))))])),


  'exceptional' : lambda self :
    x_try([(lambda : x_raise(Exception("oh no!")))],
        [(Exception, lambda e: x_print("got exception: %s" % str(e)))],
        lambda : x_print("finally!")),


  'generator' : lambda self :
    x_gen((lambda : x_yield(1)),
          (lambda : x_yield(1+1)),
          (lambda : x_yield(1+1+1))),

  "whatif" : lambda self, test :
    x_if(((lambda : test),(lambda : x_print("yep"))),
         ((lambda : True),(lambda : x_print("nope")))),


  "whatfor" : lambda self, data :
    x_block(*apply(lambda x_locals=locals() : [
      (lambda : x_assign(x_locals, "result", [])),
      (lambda : x_for(data, lambda item : x_block(
                  (lambda : x_if([(lambda : item % 2), x_continue],
                                 [(lambda : item == 9), x_break])),
                  (lambda : x_locals["result"].append(item))),
                x_for_else=(lambda : x_print("unbroken.")))),
      (lambda : x_locals["result"])])),


  "test" : lambda  self :
    x_block(
      (lambda : x_assert(self.count() is None)),
      (lambda : x_assert(self.exceptional() is None)),
      (lambda : x_assert(self.whatif(True) is None)),
      (lambda : x_assert(self.whatif(False) is None)),
      (lambda : x_assert(list(self.generator()) == [1,2,3])),
      (lambda : x_assert(list(self.whatfor(range(10))) == [0,2,4,6,8])),
      (lambda : x_assert(list(self.whatfor(range(2))) == [0])),
      (lambda : x_assert(self.dowhile() is None)))}),


lambda : x_if ([(lambda : __name__!="__main__"),
        x_block((lambda : x_assign(x_scope, "f",
                                   apply(x_scope["Functional"]))),
                (lambda : apply(getattr(x_scope["f"], "test"))))])]))

