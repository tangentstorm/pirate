#!/usr/bin/env python
#+----------------------------------------------------------------------+
#| Zend Engine                                                          |
#+----------------------------------------------------------------------+
#| Copyright (c) 1998-2002 Zend Technologies Ltd. (http://www.zend.com) |
#+----------------------------------------------------------------------+
#| This source file is subject to version 2.00 of the Zend license,     |
#| that is bundled with this package in the file LICENSE, and is        | 
#| available through the world-wide-web at the following url:           |
#| http://www.zend.com/license/2_00.txt.                                |
#| If you did not receive a copy of the Zend license and are unable to  |
#| obtain it through the world-wide-web, please send a note to          |
#| license@zend.com so we can mail you a copy immediately.              |
#+----------------------------------------------------------------------+
#| Authors: Andi Gutmans <andi@zend.com>                                |
#|          Zeev Suraski <zeev@zend.com>                                |
#+----------------------------------------------------------------------+
# $Id$
# $Log$
# Revision 1.1  2003/08/14 09:46:23  sthorne
# Added 'parrakeet' which is a parser that does (very very very) simple
# s-expressions, just to spike a php parser.
#

# Having a go writing a simple s-exp / php style thingy. really basic 
# from a parsing point of view, just to get things moving in terms of
# code generation.

from dparser import Parser
from php_tree import *

def d_start(t):
    """
start:
    statement_list
    """
    return ParseTree(t[0])

def d_statement_list(t):
    """
statement_list:
    statement*
    """
    return StatementList(t[0])

def d_statement(t):
    """
statement:
        expr
    |   echo_statement
    """
    return t[0]

def d_echo_statement(t):
    """
echo_statement: 
        '(' 'echo' expr ')'
    """
    return EchoStatement(t[2])

def d_expr(t):
    """
expr:
        string
    |   variable
    |   '(' infix_op expr expr ')'
    |   '(' expr ')'
    |   '(' 'print' expr ')'
    """
    if len(t) == 1:
        return t[0]
    if t[1] == 'print':
        return PrintExpr(t[2])
    return InfixOpExpr(t[2],t[1],t[3])

def d_infix_op(t):
    """
infix_op:
    '+' | '-' | '*' | '/' | '.'
    """
    return t[0]

def d_string(t):
    """
string:
        "'" "[^']*" "'"
    """
    return ConstString(t[1])

def d_variable(t):
    """
variable:
        '$' identifier
    """
    return Variable(t[1])

def d_identifier(t):
    """
identifier:
        "[a-zA-Z_][a-zA-Z0-9_]*"
    """
    return Identifier(t[0])

parser = Parser()

print parser.parse("(print 'abc')").emit()
