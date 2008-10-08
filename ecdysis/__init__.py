"""
ecdysis - when a snake sheds its skin :)
"""
from scheme import *
from eparse import read
#from etrans import ecompile

## runtime #######################################
##
## since ecdysis is just python without the skin,
## it's easy to compile it down to python code.
## that way, python can evaluate it for us!
##

import operator as op
from compiler import transformer

dispatch = {}

## python operator map ###########################
##
## the operator module has functions for all of
## these, but apparently no mapping table except:
##
## http://docs.python.org/lib/operator-map.html
##
## but it's easy enough to make our own
##
## @TODO: "is not",  setitem, getitem, neg, invert, not, slices

def _make_prim(op):
    prim = eval("lambda a, b: a %s b" % op)
    # reduce lets us do (+ 1 2 3 4 5) instead of just (+ 1 2)
    return lambda *seq: reduce(prim, seq)

for op in "+ - in / // & ^ | ** is << % * >> < <= == != >= >".split():
    dispatch[op] = _make_prim(op)


## lambda ########################################


def on_lambda(paramNames, body):
    print "lambda",
    print ",".join([str(p) for p in paramNames]),
    print ":",
    print _eeval_node(body)
    
    #return eval("lambda %s : %s" %
        
#    def __init__(self, head, tail):
#        return lambda head(tail)
    




special={}    
special["lambda"] = on_lambda


## evaluator #####################################
    
def _eeval_node(node):
    if isinstance(node, cons):
        form, args = node.split()
        if form in special:
            return special[form](*[a for a in args])
        else:
            # generic apply
            return dispatch[form](*[_eeval_node(a) for a in args])
    else:
        # scalar value
        return node
    
def eeval(exp):
    return _eeval_node(read(exp))

