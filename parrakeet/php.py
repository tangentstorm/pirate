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
# Revision 1.1  2003/08/13 04:13:07  sthorne
# Checking in the php grammar (php.g) and the php compiler stuff.
# Renamed php-tree to something thats a valid module name.
#
# Revision 1.8  2003/08/05 13:27:15  stephen
# The php grammar in tex format added
#
# Revision 1.7  2003/08/04 08:40:32  stephen
# just checking in before I do something drastic
#
# Revision 1.6  2003/08/03 09:53:00  stephen
# Gah. hardly any progress. more test cases.
#
# Code generation - can emit simple imcc.
#
# Revision 1.5  2003/08/03 05:58:30  stephen
# More testcases, a fixme added because associativity of expressions is broken
#
# Revision 1.4  2003/08/01 09:01:39  stephen
# Make it unittest isntead of spawn meaningless ASTs.
#
# Fixed some bugs.
#
# Revision 1.3  2003/08/01 08:31:43  stephen
# Moving the tests to a different file.
#
# Revision 1.2  2003/08/01 08:25:28  stephen
# Added the license details, as this file was derived from zend_language_parser.y
#

from dparser import Parser
from php_tree import *

def d_start(t):
    " S : statement_list "
    return ParseTree(t[0])
    
def d_identifier(t):
    """ identifier: "[a-zA-Z_][a-zA-Z0-9_]*" """
    return Identifier(t[0])

def d_statement_list(t):
    """ statement_list : top_statement* """
    return StatementList(t[0])

def d_top_statement(t):
    """ 
    top_statement : statement 
        | function_declaration_statement
        | class_declaration_statement
    """
    return t[0]

def d_while_statement(t):
    """ 
while_statement: 
        'while' '('  expr  ')' (statement | ':' statement_list 'endwhile' ';') 
    |   'do' statement 'while' '(' expr ')' ';'
    """
    if t[0] == 'do':
        return WhileStatement(t[4], t[1])
    if len(t[3]) == 1:
        return WhileStatement(t[2], t[3][0])
    return WhileStatement(t[2], t[3][1])

def d_if_statement(t):
    """ 
if_statement:
        'if' '(' expr ')' statement elseif_list else_single 
    |   'if' '(' expr ')' ':' statement_list new_elseif_list new_else_single 'endif' ';' 
    """
    if len(t) == 7:
        (_,_,expr,_,statement,elseif_list,else_statement) = t
    else:
        (_,_,expr,_,statement,_,elseif_list,else_statement,_,_) = t
    return IfStatement(expr, statement, elseif_list, else_statement)

def d_statement(t):
    """
statement:
        braced_statement
    |   if_statement
    |   while_statement
    |   switch_statement
    |   for_statement 
    |   break_statement
    |   continue_statement
    |   return_statement
    |   global_statement
    |   static_statement
    |   echo_statement
    |   expr_statement
    |   use_statement
    |   unset_statement
    |   foreach_statement
    |   declare_statement
    |   empty_statement
    |   try_catch_statement
    |   throw_statement
    """ 
    return t[0]

def d_switch_statement(t):
    """
switch_statement:
        'switch' '(' expr ')' switch_case_list 
    """
    return SwitchStatement(t[2], t[4])

def d_use_statement(t):
    """
use_statement:
        'use' use_filename ';'
    """
    return UseStatement(t[1])

def d_declare_statement(t):
    """
declare_statement:
        'declare' '(' declare_list ')' declare_statement 
    """
    return DeclareStatement(t[2], t[4])

def d_try_catch(t):
    """
try_catch_statement:
    'try' '{' statement_list '}'
        catches
    """
    return Try(t[2], t[4])

def d_catches(t):
    """ 
catches: ( 'catch' '(' fully_qualified_class_name variable ')' '{' statement_list '}' )+
    """
    return [Catch(x[2], x[3], x[6]) for x in t]

def d_throw_statement(t):
    """ throw_statement: 'throw' expr ';' """
    return Throw(t[1])
    
def d_empty_statement(t):
    """ empty_statement: ';' """
    return Empty()

def d_braced_statement(t):
    """
braced_statement:
    '{' statement_list '}'
    """
    return t[1]

def d_expr_statement(t):
    """ expr_statement: expr ';' """
    return ExprStatement(t[0])

def d_continue_statement(t):
    """ continue_statement: 'continue' expr? ';' """
    if t[1] == ';':
        return ContinueStatement(None)
    return ContinueStatement(t[1])

def d_break_statement(t):
    """ break_statement: 'break' expr? ';' """
    if t[1] == ';':
        return BreakStatement(None)
    return BreakStatement(t[1])

def d_return_statement(t):
    """ return_statement: 'return' (variable | expr_without_variable )? ';' """
    if len(t[1]) == 0:
        return ReturnStatement(None)
    return ReturnStatement(t[1][0])

def d_print(t):
    """ print_expr: 'print' expr """
    return PrintExpr(t[1])

def d_echo(t):
    """ echo_statement: 'echo' echo_expr_list ';' """
    return EchoStatement(t[1])

def d_unset_statement(t):
    """ unset_statement: 'unset' '(' unset_variables ')' ';' """
    return UnsetStatement(t[2])

def d_unset_variables(t):
    """ unset_variables: ((variable ',')* variable)? """
    if len(t[0]) == 2:
        return [x[0] for x in t[0][0]].append(t[0][1])
    return t[0]

def d_const_string(t):
    r""" const_string : '"' "[^\"]" '"' """
    return ConstString(t[1])

def d_use_filename(t):
    """ use_filename: const_string | '(' const_string ')' """
    if len(t) == 3:
        return t[1]
    return t[0]

# Should encapsualte this.
def d_class_entry_type(t):
    """
class_entry_type:
        'class'
    |   'abstract' 'class'
    |   'final' 'class'
    |   'interface'
    """
    return t

def d_function_declaration_statement(t):
    """
function_declaration_statement:
        'function' '&'? identifier '(' parameter_list ')' '{' statement_list '}' 
    """
    return Function(t[2], t[4], t[7], len(t[1]))

def d_extends_from(t):
    """ extends_from: ('extends' fully_qualified_class_name)? """
    if len(t[0]) > 1:
        return t[0][1]
    return None

def d_unticked_class_declaration_statement(t):
    """
class_declaration_statement:
        class_entry_type identifier extends_from
            implements_list
            '{' class_statement_list '}'
    """
    return Class(t[1], t[0], t[2], t[3], t[5])

def d_implements_list(t):
    """ implements_list: ('implements' interface_list)? """
    return [x[1] for x in t]

def d_interface_list(t):
    """ interface_list: (fully_qualified_class_name ',')* fully_qualified_class_name """
    return t[::2]

def d_for_statement(t):
    """ 
for_statement: 
    'for' '(' for_expr ';' for_expr ';' for_expr ')' 
        (statement |  ':' statement_list 'endfor' ';') 
    """
    if len(t) == 9:
        (_,_,expr1,_,expr2,_,expr3,_,statement) = t
    else:
        (_,_,expr1,_,expr2,_,expr3,_,_,statement,_,_) = t
    return ForStatement(expr1, expr2, expr3, statement)

def d_foreach_statement(t):
    """ foreach_statement: 
    'foreach' '(' expr 'as' foreach_optional_arg w_variable ')'
    (statement |  ':' statement_list 'endforeach' ';') 
    """
    if len(t[7]) == 1:
        return ForeachStatement(t[2], t[4], t[5], t[7][0])
    elif len(t[7]) == 4:
        return ForeachStatement(t[2], t[4], t[5], t[7][1])

def d_foreach_optional_arg(t):
    """
foreach_optional_arg:
    (w_variable '=>')?
    """
    if len(t[0]) > 1:
        return t[0][0]
    return None

def d_declare_statement(t):
    """ declare_statement: statement |  ':' statement_list 'enddeclare' ';' """
    if len(t) > 1:
        return t[1]
    return t[0]

def d_declare_list(t):
    """
declare_list:
        identifier '=' scalar
    |   declare_list ',' identifier '=' scalar
    """
    if t[1] == '=':
        return [Declaration(t[0], t[2])]
    return t[0].append(Declaration(t[2], t[4]))

def d_switch_case_list(t):
    """
switch_case_list:
        '{' ';'? case_list '}'
    |   ':' ';'? case_list 'endswitch' ';'
    """
    return t[2]

def d_case_list(t):
    """
case_list:
    | case_list 'case' expr case_separator statement_list 
    | case_list 'default' case_separator statement_list 
    """
    if t == []:
        return []
    if t[2] == ':':
        return t[0].extend([DefaultCase(), t[3]])
    return t[0].extend([Case(t[2]), t[4]])

def d_case_separator(t):
    """ case_separator: ':' | ';' """
    return ':'

def d_elseif_list(t):
    """ elseif_list: ( 'elseif' '(' expr ')' statement )* """
    if len(t[0]):
        return [ElseIf(i[2], i[4]) for i in t]
    return []

def d_new_elseif_list(t):
    """ new_elseif_list : ('elseif' '(' expr ')' ':' statement_list)* """
    if len(t[0]):
        return [ElseIf(i[2], i[5]) for i in t]
    return []

def d_else_single(t):
    """ else_single: ('else' statement)? """
    if len(t[0]):
        return t[0][1]
    return None

def d_new_else_single(t):
    """ new_else_single: ('else' ':' statement_list)? """
    if len(t[0]):
        return t[0][2]
    return None

def d_parameter_list(t):
    """ parameter_list: ((non_empty_parameter ',')* non_empty_parameter)? """
    if len(t[0]) == 2:
        return [t[0][0], t[0][1]]
    if len(t[0]) == 1:
        return [t[0][0]]
    return []

def d_non_empty_parameter(t):
    """
non_empty_parameter:
        optional_class_type T_VARIABLE  
    |   optional_class_type '&' T_VARIABLE  
    |   optional_class_type '&' T_VARIABLE '=' scalar 
    |   'const' optional_class_type T_VARIABLE  
    |   optional_class_type T_VARIABLE '=' scalar    
    """
    if len(t) == 2:
        return Parameter(t[1], t[0])
    elif len(t) == 3 and t[1] == '&':
        return Reference(Parameter(t[2], t[0]))
    elif len(t) == 3 and t[0] == 'const':
        return Reference(Parameter(t[2], t[1]))
    elif len(t) == 5:
        return Reference(Parameter(t[2], t[0], t[4]))
    else:
        return Parameter(t[1], t[0], t[3])

def d_optional_class_type(t):
    """ optional_class_type: fully_qualified_class_name? """
    return t[0]

def d_function_call_parameter_list(t):
    """ function_call_parameter_list: ((function_call_parameter ',')* function_call_parameter)? """
    return t[0][::2]

def d_non_empty_function_call_parameter_list(t):
    """
function_call_parameter:
        expr_without_variable
    |   variable
    |   '&' w_variable 
    """
    if t[0] == '&':
        return Reference(t[1])
    return t[0]

def d_global_statement(t):
    """ global_statement: 'global' global_var_list ';' """
    return GlobalStatement(t[1])

def d_global_var_list(t):
    """ global_var_list: (global_var ',')* global_var """
    l = t[0][::2]
    l.append(t[1])
    return l

# Really yucky. VariableVariable
def d_global_var(t):
    """
global_var:
    T_VARIABLE
    |   '$' r_variable
    |   '$' '{' expr '}'
    """
    if len(t) == 4:
        return VariableVariable(t[2])
    elif len(t) == 2:
        return VariableVariable(t[1])
    else:
        return t[0]

def d_static_statement(t):
    """ static_statement: 'static' static_var_list ';' """
    return StaticStatement(t[1])

def d_static_var_list(t):
    """
static_var_list:
        (static_var ',')* static_var
    """
    l = t[0][::2]
    l.append(t[1])
    return l

def d_static_var(t):
    ''' static_var : T_VARIABLE ('=' scalar)?'''
    if len(t[1]) > 0:
        return StaticVariable(t[0], t[1][1])
    return StaticVariable(t[0])

def d_class_statement_list(t):
    """ class_statement_list: class_statement* """
    return t

def d_class_statement(t):
    """
class_statement:
        variable_modifiers class_variable_declaration ';'
    |   class_constant_declaration ';'
    |   method_modifiers 'function' is_reference identifier '(' parameter_list ')' method_body
    """
    return t

def d_method_body(t):
    """ method_body: ';' |  '{' statement_list '}' """
    if t[0] == ';':
        return Empty()
    return t[1]

def d_variable_modifiers(t):
    """ variable_modifiers: member_modifier+ | 'var' """
    return t[0]

def d_method_modifiers(t):
    """ method_modifiers: member_modifier* """
    return t[0]

def d_member_modifier(t):
    """ member_modifier: 'public' | 'protected' | 'private' | 'static' | 'abstract' | 'final' """
    return t[0]

def d_class_variable_declaration(t):
    """
class_variable_declaration:
        (class_variable ',')* class_variable
    """
    l = t[0][::2]
    l.append(t[1])
    return ListTuple(l)

def d_class_variable(t):
    """ class_variable: variable |   variable '=' scalar """
    if len(t) == 1:
        return ClassVariable(t[0])
    return ClassVariable(t[0], t[2])


def d_class_constant_declaration(t):
    """
class_constant_declaration:
        class_constant_declaration ',' identifier '=' expr
    |   'const' identifier '=' expr
    """
    return t

def d_echo_expr_list(t):
    """ echo_expr_list: ((expr ',')* expr)? """
    return t[0][::2]

def d_for_expr(t):
    """ for_expr: ((expr ',')* expr)? """
    return t[0][::2]

def d_expr_without_variable(t):
    """
expr_without_variable:  
        list_assignment_expr
    |   new_expr
    |   assignment_expr
    |   in_place_expr
    |   post_expr
    |   pre_expr
    |   infix_op_expr
    |   prefix_op_expr
    |   comparison_expr
    |   instanceof_expr
    |   parens_expr
    |   conditional_expr
    |   internal_functions_in_yacc
    |   cast_expr
    |   suppress_expr
    |   exit_expr
    |   scalar
    |   array_expr
    |   print_expr
    |   backtick_expr
    """
    if len(t) == 1:
        return t[0]
    return t

def d_suppress_expr(t):
   """ suppress_expr: '@' expr """
   return SuppressExpr(t[1])

def d_prefix_op_expr(t):
    """
prefix_op_expr: ('+' | '-'  | '!' | '~') expr
    """ 
    return PrefixOpExpr(t[0][0], t[1][0])

# FIXME: this desperately needs to be refactored
# so that 1 + 2 * 3 != (1+3)*3
def d_op_expr(t):
    """
infix_op_expr:
       expr op expr
    """
    print t
    return InfixOpExpr(t[0], t[1][0], t[2])

def d_op(t):
    """
op:
        '||' $binary_op_left 1
    |   '&&' $binary_op_left 2
    |   'or' $binary_op_left 3
    |   'and' $binary_op_left 4
    |   'xor' $binary_op_left 5
    |   '|' $binary_op_left 6
    |   '&' $binary_op_left 7
    |   '^' $binary_op_left 8
    |   '.' $binary_op_left 9
    |   '+' $binary_op_left 10
    |   '-' $binary_op_left 11
    |   '*' $binary_op_left 12
    |   '/' $binary_op_left 13
    |   '%' $binary_op_left 14
    |   '<<' $binary_op_left 15
    |   '>>' $binary_op_left 16
    """
    return t[0]
    

def d_conditional_expr(t):
    """
    conditional_expr:  expr '?' expr ':' expr
    """
    return TurnaryExpr(t[0], t[2], t[4])

def d_cast_expr(t):
    """
    cast_expr:   '(' type ')' expr
    """
    return CastExpr(t[1], t[3])

def d_instanceof_expr(t):
    """ instanceof_expr: expr 'instanceof' class_name_reference """
    return InstanceofExpr(t[0], t[2])

def d_parens_expr(t):
    """ parens_expr: '(' expr ')' """
    return t[1]

def d_post_expr(t):
    """ post_expr: rw_variable ('++' | '--')  """
    return PostExpr(t[0], t[1])
    
def d_pre_expr(t):
    """ pre_expr: ('++' | '--') rw_variable """
    return PreExpr(t[0], t[1])

def d_list_assignment_expr(t):
    ''' list_assignment_expr: 'list' '(' assignment_list ')' '=' expr '''
    return ListAssignmentExpr(t[2], t[5])

def d_new_expr(t):
    """ new_expr:  'new' class_name_reference ctor_arguments """
    return t[1],t[2]

def d_assignment_expr(t):
    """
assignment_expr:
        variable '=' expr
    |   variable '=' '&' variable
    |   variable '=' '&' new_expr
    """
    if len(t) == 3:
        return AssignmentExpr(t[0], t[2])
    return AssignmentExpr(t[0], Reference(t[3]))

def d_backtick_expr(t):
    """ backtick_expr: '`' encaps_list '`' """
    return BacktickExpr(t[1])

def d_in_place(t):
    """ 
in_place_expr:
    variable ('+=' |  '-=' |  '*=' |  '/=' |  '.=' |  '%=' |  '&=' |  '|=' |  '^=' |  '<<=' |  '>>=') expr
    """
    return InPlaceExpr(t[0],t[1],t[2])

def d_comparison(t):
    """ comparison_expr: expr ('===' | '!==' | '==' | '!=' | '<' | '<=' | '>' | '>=') expr """
    return Comparison(t[1][0],t[0],t[2])

def d_type(t):
    """ type: 'int' | 'double' | 'string' | 'array' | 'object' | 'bool' | 'boolean' | 'unset' """
    return t[0]

def d_function_call(t):
    """
function_call:
        identifier '(' function_call_parameter_list ')' 
    |   class_constant '(' function_call_parameter_list ')' 
    |   variable_without_objects  '(' function_call_parameter_list ')' 
    """
    return FunctionCall(t[0], t[2])

def d_fully_qualified_class_name(t):
    """ fully_qualified_class_name: identifier """
    return t[0]

def d_class_name_reference(t):
    """ class_name_reference: identifier | dynamic_class_name_reference """
    return t[0]

def d_dynamic_class_name_reference(t):
    """
dynamic_class_name_reference:
        base_variable ('->' object_property)*
    """
    if len(t) == 1:
        return t[0]
    return t

def d_exit_expr(t):
    """ exit_expr: 'exit' ('(' expr? ')')?  """
    if len(t[1])>0 and t[1][1] != ')':
        return ExitExpr(t[1][1])
    return ExitExpr()

def d_ctor_arguments(t):
    """ ctor_arguments: ('(' function_call_parameter_list ')')? """
    if len(t[0]) == 0:
        return None
    return t[0][1]

def d_number(t):
    """
number:
    "[0-9]+"
    |   "[0-9]*[\.][0-9]+"
    |   "[0-9]+[\.][0-9]*"
    """
    return Number(t[0])

def d_common_scalar(t):
    r"""
common_scalar:
    number
    |   '"' "[a-zA-Z0-9_ \t\\]*" '"'
    |   "'[a-zA-Z0-9_ \t\\]*'"
    |   '__LINE__'
    |   '__FILE__'
    |   '__CLASS__'
    |   '__FUNCTION__'
    """
    if len(t) == 3:
        return ConstString(t[1])
    return t[0]

def d_static_class_constant(t):
    """ static_class_constant: identifier '::' identifier """
    return t

def d_scalar(t):
    """
scalar:
        identifier
    |   class_constant
    |   string
    |   common_scalar
    """
    return t[0]

def d_string(t):
    """
string:
        '"' encaps_list '"'
    |   "'" encaps_list "'"
    """
    return String(t[1])

def d_expr(t):
    """ expr: r_variable | expr_without_variable """
    return t[0]

def d_r_variable(t):
    """ r_variable: variable """
    return t[0]

def d_w_variable(t):
    """ w_variable: variable """
    return t[0]

def d_rw_variable(t):
    """ rw_variable: variable """
    return t[0]

def d_variable(t):
    """
variable:
        base_variable_with_function_calls 
        ('->' object_property method_or_not)*
    """
    v = t[0]
    for (property, parameters) in zip(t[1][1::3],t[1][2::3]):
        if not parameters == None:
            v = ObjectCall(v, property, parameters)
        else:
            v = ObjectVariable(v, property)
    return v

def d_method_or_not(t):
    """ method_or_not: ( '(' function_call_parameter_list ')' )? """
    if len(t[0]) > 0:
        return t[0][1]
    return None

def d_variable_without_objects(t):
    """ 
variable_without_objects:
        reference_variable
    |   indirect_reference
    """
    return t[0]

def d_static_member(t):
    """
static_member:
        fully_qualified_class_name '::' variable_without_objects 
    """
    return t

def d_base_variable_with_function_calls(t):
    """
base_variable_with_function_calls:
        base_variable
    |   function_call
    """
    return t[0]

def d_base_variable(t):
    """
base_variable:
        reference_variable 
    |   indirect_reference
    |   static_member 
    """
    return t[0]
    
def d_reference_variable(t):
    """
reference_variable:
        reference_variable '[' dim_offset ']'
    |   reference_variable '{' expr '}'
    |   compound_variable
    """
    if len(t) == 1:
        return t[0]
    return t

def d_compound_variable(t):
    """
compound_variable:
        T_VARIABLE
    |   '$' '{' expr '}'
    """
    if len(t) == 1:
        return t[0]
    return VariableVariable(t[2])

def d_dim_offset(t):
    """ dim_offset: expr? """
    return t[0]

def d_object_property(t):
    """ object_property: object_dim_list | variable_without_objects """
    return t[0]

def d_object_dim_list(t):
    """
object_dim_list:
        object_dim_list '[' dim_offset ']'
    |   object_dim_list '{' expr '}'
    |   variable_name 
    """
    return t[0]

def d_variable_name(t):
    """ variable_name: identifier | '{' expr '}' """
    if t[0] == '{':
        return t[1]
    return t[0]

def d_simple_indirect_reference(t):
    """ indirect_reference: '$' (indirect_reference | variable)"""
    return VariableVariable(t[1][0])

def d_assignment_list(t):
    """
assignment_list:
        (assignment_list_element ',')* assignment_list_element
    """
    l = t[0][::2]
    l.append(t[1])
    return ListTuple(l)

def d_assignment_list_element(t):
    """
assignment_list_element:
        variable
    |   'list' '(' assignment_list ')'  
    |
    """
    if len(t) == 0:
        return Empty()
    elif len(t) == 1:
        return t[0]
    return t[2]

def d_array(t):
    """
array_expr:
    'array' '(' ((array_pair_list ',')* array_pair_list ','?)? ')'
    """
    return Array(t[2][::2])

def d_array_pair_list(t):
    """
array_pair_list:
        expr '=>' expr
    |   expr
    |   expr '=>' '&' w_variable 
    |   '&' w_variable 
    """
    if len(t) == 4:
        return ArrayPair(t[0], Reference(t[3]))
    elif len(t) == 3:
        return ArrayPair(t[0], t[2])
    elif len(t) == 2:
        return ArraySingle(Reference(t[1]))
    else:
        return ArraySingle(t[0])

def d_encaps_var(t):
    """
encaps_var:
        variable 
    |   variable '['  encaps_var_offset ']' 
    |   variable '->' identifier 
    |   '${' expr '}' 
    |   '${' identifier '[' expr ']' '}' 
    |   '{' variable '}'
    """
    return t

def d_encaps_list(t):
    """
encaps_list:
        encaps_list (encaps_var | identifier | number |
                /* T_ENCAPSED_AND_WHITESPACE | T_CHARACTER | T_BAD_CHARACTER | */
                '[' | ']' | '{' | '}' | '->')?
    """
    return t

def d_encaps_var_offset(t):
    """
encaps_var_offset:
        identifier
    |   number
    |   variable
    """
    return t

def d_internal_functions_in_yacc(t):
    """
internal_functions_in_yacc:
        'isset' '(' isset_variables ')'
    |   'empty' '(' variable ')'
    |   'include' expr 
    |   'include_once' expr
    |   'eval' '(' expr ')' 
    |   'require' expr
    |   'require_once' expr
    """
    return t

def d_isset_variables(t):
    """ isset_variables: (variable ',')* variable """
    l = t[0][::2]
    l.append(t[1])
    return l

def d_class_constant(t):
    """ class_constant: fully_qualified_class_name '::' identifier """
    return t

def d_is_reference(t):
    """ is_reference: '&'? """
    return t

def d_T_VARIABLE(t):
    """ T_VARIABLE: '$' identifier """
    return Variable(t[1])

def d_whitespace(t):
    r""" whitespace: (blank | comment)* """
   
def d_comment(t):
    r""" comment: '//' "[a-zA-Z0-9 \t\r]*\n" || '/*' "[a-zA-Z0-9 \t\r\n]" '*/' """

def d_blank(t):
    r""" blank: "[ \t\r\n]" """

parser = Parser()
