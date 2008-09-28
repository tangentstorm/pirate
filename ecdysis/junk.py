

import ast2py

code = """\

import os as eep, sys

class foo(object):
    'an object'
    def bar(self, a, b):
        return lispplus(a, b)

"""

"""
    def f (self, a, b):
        return a + b
    def g (self, a, b):
        return a - b
"""

#code = open("tests/classes").read()


import compiler 
tree = compiler.parse(code)
#print tree


print ast2py.ast2py(tree)


#tree.filename = "whatever"
#code = compiler.pycodegen.ModuleCodeGenerator(tree).getCode()
#exec code

import wx

app = wx.App()
f = wx.Frame(None, title="some code")
f.Show()
s = wx.SoundFromData(open("Nice_Not-NEO_Soun-1374.wav", "rb").read())
s.Play(wx.SOUND_ASYNC)
app.MainLoop()
