from ast2py import *

class LambdaFier(Deparser):

    pass








for line in visit(compiler.parse(open("functional.py").read()), LambdaFier):
    print line
