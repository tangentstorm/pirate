

class ParseTree:
    def __init__(self, tree):
        self.tree = tree

    def __repr__(self):
        return '<ParseTree contents:\n%r>' % self.tree

    def emit(self):
        return '.sub _MAIN\n' + self.tree.emit() + '\tend\n.end\n'

        
class Identifier:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '<Identifier name:%r>' % self.name

class StatementList:
    def __init__(self, statement_list):
        self.statement_list = statement_list

    def __repr__(self):
        return '<StatementList len:%d contents:\n%r>' % (len(self.statement_list), self.statement_list)

    def emit(self):
        return ''.join(['\t' + x.emit() + '\n' for x in self.statement_list])

class WhileStatement:
    def __init__(self, expr, statement):
        self.expr = expr
        self.statement = statement

    def __repr__(self):
        return '<WhileStatement\nexpr:%r\nstatement:%r>' % (self.expr, self.statement)

class SwitchStatement:
    def __init__(self, expr, case_list):
        self.expr = expr
        self.case_list = case_list

    def __repr__(self):
        return '<SwitchStatement\nexpr:%r\ncase_list:%r>' % (self.expr, self.case_list)

class IfStatement:
    def __init__(self, expr, statement, elseif_list, else_statement):
        self.expr = expr
        self.statement = statement
        self.elseif_list = elseif_list
        self.else_statement = else_statement

    def __repr__(self):
        return '<IfStatement\nexpr:%r\nstatement:%r\nelsieif:%r\nelse:%r>' % (self.expr, self.statement, self.elseif_list, self.else_statement)

class DeclareStatement:
    def __init__(self, declare_list, statement):
        (self.declare_list, self.statement) = (declare_list, statement)

    def __repr__(self):
        return '<Declare (%r)\n%r>' % (self.declare_list, self.statement)

class UseStatement:
    def __init__(self, filename):
        self.filename = filename

    def __repr__(self):
        return '<Use file:%r>' % self.filename

class Try:
    def __init__(self, statement_list, catches):
        (self.statement_list, self.catches) = (statement_list, catches)

    def __repr__(self):
        return '<Try %r\n%r>' % (self.statement_list, self.catches)
    
class Catch:
    def __init__(self, type, variable, statement_list):
        (self.type, self.variable, self.statement_list) = (type, variable, statement_list)

    def __repr__(self):
        return '<Catch %r as %r\n%r>' % (self.type, self.variable, self.statement_list)

class Throw:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<Throw %r>' % self.expr

class Empty:
    def __repr__(self):
        return '<Empty>'

class DefaultCase:
    def __repr__(self):
        return '<Default>'

class Case:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<Case %r>' % self.expr

class ExprStatement:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<Expr %r>' % self.expr

    def emit(self):
        return '\t' + self.expr.emit() + '\n'

class PostExpr:
    def __init__(self, var, op):
        (self.var, self.op) = (var, op)

    def __repr__(self):
        return '<Post %r%r>' % (self.var, self.op)

class PreExpr:
    def __init__(self, op, var):
        (self.op, self.var) = (op, var)

    def __repr__(self):
        return '<Pre %r%r>' % (self.op, self.var)

class InstanceofExpr:
    def __init__(self, expr, classname):
        (self.expr, self.classname) = (expr, classname)

    def __repr__(self):
        return '<Instanceof %r %r>' % (self.expr, self.classname)

class ContinueStatement:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<Continue %r>' % self.expr

class BreakStatement:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<Break %r>' % self.expr

class ReturnStatement:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<Return %r>' % self.expr

class EchoStatement:
    def __init__(self, expr_list):
        self.print_list = [PrintExpr(e) for e in expr_list]

    def __repr__(self):
        return '<Echo %r>' % self.print_list

    def emit(self):
        return [p.emit() for p in self.print_list]
        

class PrintExpr:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<Print %r>' % self.expr

    def emit(self): 
        return 'print ' + self.expr.emit()

class UnsetStatement:
    def __init__(self, variables):
        self.variables = variables

    def __repr__(self):
        return '<Unset %r>' % self.variables

class String:
    def __init__(self, encaps):
        self.encaps = encaps

    def __repr__(self):
        return '<String %r>' % self.encaps

    def emit(self):
        return '"' + [x.emit() for x in self.encaps] + '"'

class ConstString:
    def __init__(self, contents):
        self.contents = contents

    def __repr__(self):
        return '<ConstString %r>' % self.contents

    def emit(self):
        return '"' + self.contents + '"'

class Function:
    def __init__(self, name, params, statement_list, reference = 0):
        (self.name, self.params, self.statement_list, self.reference) = \
            (name, params, statement_list, reference)

    def __repr__(self):
        return '<Func\n%s%s(%s)\nstatementlist:%s>' % \
            (('','&')[self.reference], self.name, self.params, self.statement_list)

class Class:
    def __init__(self, name, type, extends, implements, statementlist):
        (self.name, self.type, self.extends, self.implements, self.statementlist) = \
            (name, type, extends, implements, statementlist)

    def __repr__(self):
        return '<Class %r %r %r %r %r>' % \
            (self.name, self.type, self.extends, self.implements, self.statementlist)

class ForStatement:
    def __init__(self, expr1, expr2, expr3, statement):
        self.expr1 = expr1
        self.expr2 = expr2
        self.expr3 = expr3
        self.statement = statement

    def __repr__(self):
        return '<ForStatement\nexpr1:%r\nexpr2:%r\nexpr3:%r\nstatement:%r>' % (self.expr1, self.expr2, self.expr3, self.statement)

class ForeachStatement:
    def __init__(self, array, key, value, statement):
        (self.array, self.key, self.value, self.statement) = (array, key, value, statement)

    def __repr__(self):
        return '<Foreach\narray:%r\nkey:%r\nvalue:%r\nstatement:%r>' % (self.array,self.key, self.value, self.statement)

class Declaration:
    def __init__(self, identifier, scalar):
        (self.identifier, self.scalar) = (identifier, scalar)

    def __repr__(self):
        return '<Declaration %r = %r>' % (self.identifier, self.scalar)

class ElseIf:
    def __init__(self, expr, statement):
        self.expr = expr
        self.statement = statement

    def __repr__(self):
        return '<ElseIf %r\n%r>' % (self.expr, self.statement)

class Parameter:
    def __init__(self, variable, opt_type, default=None):
        self.variable = variable
        self.opt_type = opt_type
        self.default = default

    def __repr__(self):
        return '<Parm %r %r %r>' % (self.opt_type, self.variable, self.default)
        
class Argument:
    def __init__(self, arg):
        self.arg = arg

    def __repr__(self):
        return "<Arg %r>" % self.arg
        
class Reference:
    def __init__(self, variable):
        self.variable = variable

    def __repr__(self):
        return "<Ref %r>" % self.variable

class SuppressExpr:
    def __init__(self, variable):
        self.variable = variable

    def __repr__(self):
        return "<Suppress %r>" % self.variable

class GlobalStatement:
    def __init__(self, var_list):
        self.var_list = var_list

    def __repr__(self):
        return '<Global %r>' % self.var_list

class StaticStatement:
    def __init__(self, var_list):
        self.var_list = var_list

    def __repr__(self):
        return '<Static %r>' % self.var_list

class StaticVariable:
    def __init__(self, variable, default=None):
        self.variable, self.default = variable, default

    def __repr__(self):
        return '<StaticVariable %r = %r>' % (self.variable, self.default)

class PrefixOpExpr:
    def __init__(self, op, rhs):
        self.op, self.rhs = op, rhs

    def __repr__(self):
        return '<PrefixOp %r %r>' % (self.op , self.rhs)

class AssignmentExpr:
    def __init__(self, lhs, rhs):
        self.lhs, self.rhs = lhs, rhs

    def __repr__(self):
        return '<Assignment %r = %r>' % (self.lhs , self.rhs)

class InfixOpExpr:
    def __init__(self, lhs, op, rhs):
        self.lhs, self.op, self.rhs = lhs, op, rhs

    def __repr__(self):
        return '<InFixOp %r %r %r>' % (self.lhs , self.op, self.rhs)

    def emit(self):
        return ' '.join([self.lhs.emit(), self.op, self.rhs.emit()])

class ListAssignmentExpr:
    def __init__(self, lhs, rhs=None):
        self.lhs, self.rhs = lhs, rhs

    def __repr__(self):
        return '<ListAssignment %r = %r>' % (self.lhs , self.rhs)

class CastExpr:
    def __init__(self, type, expr):
        self.type, self.expr = type, expr

    def __repr__(self):
        return '<Cast (%r)%r>' % (self.type , self.expr)

class ListTuple:
    def __init__(self, contents):
        self.contents = contents

    def __repr__(self):
        return '<Tuple %r>' % self.contents
    
class BacktickExpr:
    def __init__(self, encaps):
        self.encaps = encaps

    def __repr__(self):
        return '<Backtick %r>' % self.encaps
    
class InPlaceExpr:
    def __init__(self, lvar, op, expr):
        (self.lvar, self.op, self.expr) = (lvar, op, expr)

    def __repr__(self):
        return '<InPlace\nLVal:%r\nOp:%r\nExpr:%r>' % (self.lvar, self.op, self.expr)

class Comparison:
    def __init__(self, type, expr1, expr2):
        (self.type, self.expr1, self.expr2) = (type, expr1, expr2)

    def __repr__(self):
        return '<Comparison\ntype:%r\nLHS:%r\nRHS:%r>' % (self.type, self.expr1, self.expr2)

class VariableVariable:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return "<VariableVariable %r>" % self.expr

class Variable:
    def __init__(self, ident):
        self.ident = ident

    def __repr__(self):
        return "<Variable %s>" % self.ident

class Array:
    def __init__(self, pair_list):
        self.pair_list = pair_list

    def __repr__(self):
        return '<Array %r>' % self.pair_list

class ArraySingle:
    def __init__(self, expr):
        self.expr = expr

    def __repr__(self):
        return '<ArraySingle %r>' % (self.expr)

class ArrayPair:
    def __init__(self, expr1, expr2):
        (self.expr1, self.expr2) = (expr1, expr2)

    def __repr__(self):
        return '<ArrayPair %r=>%r>' % (self.expr1, self.expr2)

class ExitExpr:
    def __init__(self, expr=None):
        self.expr = expr

    def __repr__(self):
        return '<Exit %r>' % self.expr

class ClassVariable:
    def __init__(self, variable, default=None):
        self.variable, self.default = variable, default

    def __repr__(self):
        return '<ClassVariable %r = %r>' % (self.variable, self.default)

class ObjectVariable:
    def __init__(self, object, property):
        self.object, self.property = object, property

    def __repr__(self):
        return '<ObjectVariable %r -> %r>' % (self.object, self.property)

class FunctionCall:
    def __init__(self, function, parameters):
        self.function , self.parameters = function, parameters

    def __repr__(self):
        return '<FunctionCall %r %r>' % (self.function, self.parameters)

class ObjectCall:
    def __init__(self, object, method, parameters):
        self.object, self.method , self.parameters = object, method, parameters

    def __repr__(self):
        return '<ObjectCall %r -> %r %r>' % (self.object, self.method, self.parameters)

class TurnaryExpr:
    def __init__(self, expr1, expr2, expr3):
        (self.expr1, self.expr2, self.expr3) = (expr1, expr2,expr3)

    def __repr__(self):
        return '<Turnary %r ? %r : %r>' % (self.expr1, self.expr2, self.expr3)

class Number:
    def __init__(self, num):
        self.num = num

    def __repr__(self):
        return '<Number %r>' % self.num

    def emit(self):
        return self.num

