/*
   +----------------------------------------------------------------------+
   | Zend Engine                                                          |
   +----------------------------------------------------------------------+
   | Copyright (c) 1998-2002 Zend Technologies Ltd. (http://www.zend.com) |
   +----------------------------------------------------------------------+
   | This source file is subject to version 2.00 of the Zend license,     |
   | that is bundled with this package in the file LICENSE, and is        | 
   | available at through the world-wide-web at                           |
   | http://www.zend.com/license/2_00.txt.                                |
   | If you did not receive a copy of the Zend license and are unable to  |
   | obtain it through the world-wide-web, please send a note to          |
   | license@zend.com so we can mail you a copy immediately.              |
   +----------------------------------------------------------------------+
   | Authors: Andi Gutmans <andi@zend.com>                                |
   |          Zeev Suraski <zeev@zend.com>                                |
   +----------------------------------------------------------------------+
*/

start:
	top_statement_list
;

top_statement_list:
		top_statement_list   top_statement 
	|	/* empty */
;

top_statement:
		statement
	|	declaration_statement	
;

inner_statement_list:
		inner_statement_list   inner_statement 
	|	/* empty */
;

inner_statement:
		statement
	|	declaration_statement
;

statement:
		unticked_statement 
;

unticked_statement:
		'{' inner_statement_list '}'
	|	'if' '(' expr ')'  statement  elseif_list else_single 
	|	'if' '(' expr ')' ':'  inner_statement_list  new_elseif_list new_else_single 'endif' ';' 
	|	'while' '('  expr  ')'  while_statement 
	|	'do'  statement 'while' '('  expr ')' ';' 
	|	'for' 
			'('
				for_expr
			';' 
				for_expr
			';' 
				for_expr
			')' 
			for_statement 
	|	'switch' '(' expr ')'	 switch_case_list 
	|	'break' ';'				
	|	'break' expr ';'		
	|	'continue' ';'			
	|	'continue' expr ';'		
	|	'return' ';'						
	|	'return' expr_without_variable ';'	
	|	'return' cvar ';'					
	|	'global' global_var_list ';'
	|	'static' static_var_list ';'
	|	'echo' echo_expr_list ';'
	|	T_INLINE_HTML			
	|	expr ';'				
	|	'use' use_filename ';'		
	|	'unset' '(' unset_variables ')' ';'
	|	'foreach' '(' w_cvar 'as' w_cvar foreach_optional_arg ')'  foreach_statement 
	|	'foreach' '(' expr_without_variable 'as' w_cvar foreach_optional_arg ')'  foreach_statement 
	|	'declare'  '(' declare_list ')' declare_statement 
	|	';'		/* empty statement */
;

unset_variables:
		unset_variable
	|	unset_variables ',' unset_variable
;

unset_variable:
		cvar	
;

use_filename:
		T_CONSTANT_ENCAPSED_STRING			
	|	'(' T_CONSTANT_ENCAPSED_STRING ')'	
;

declaration_statement:
		unticked_declaration_statement	
;

unticked_declaration_statement:
		'function' is_reference T_STRING 
			'(' parameter_list ')' '{' inner_statement_list '}' 
	|	'function' is_reference T_STRING  
			parameter_list '(' inner_statement_list ')' ';' 
	|	'class' T_STRING  '{' class_statement_list '}' 
	|	'class' T_STRING 'extends' T_STRING  '{' class_statement_list '}' 
;

foreach_optional_arg:
		/* empty */				
	|	'=>' w_cvar	
;

for_statement:
		statement
	|	':' inner_statement_list 'endfor' ';'
;


foreach_statement:
		statement
	|	':' inner_statement_list 'endforeach' ';'
;

declare_statement:
		statement
	|	':' inner_statement_list 'enddeclare' ';'
;

declare_list:
		T_STRING '=' static_scalar					
	|	declare_list ',' T_STRING '=' static_scalar	
;

switch_case_list:
		'{' case_list '}'					
	|	'{' ';' case_list '}'				
	|	':' case_list 'endswitch' ';'		
	|	':' ';' case_list 'endswitch' ';'	
;

case_list:
		/* empty */	
	|	case_list 'case' expr case_separator  inner_statement_list 
	|	case_list 'default' case_separator  inner_statement_list 
;

case_separator:
		':'
	|	';'
;

while_statement:
		statement
	|	':' inner_statement_list 'endwhile' ';'
;

elseif_list:
		/* empty */
	|	elseif_list 'elseif' '(' expr ')'  statement 
;

new_elseif_list:
		/* empty */
	|	new_elseif_list 'elseif' '(' expr ')' ':'  inner_statement_list 
;

else_single:
		/* empty */
	|	'else' statement
;

new_else_single:
		/* empty */
	|	'else' ':' inner_statement_list
;

parameter_list: 
		non_empty_parameter_list
	|	/* empty */
;

non_empty_parameter_list:
		T_VARIABLE				
	|	'&' T_VARIABLE			
	|	'const' T_VARIABLE 		
	|	T_VARIABLE '=' static_scalar				
	|	non_empty_parameter_list ',' T_VARIABLE 	
	|	non_empty_parameter_list ',' '&' T_VARIABLE	
	|	non_empty_parameter_list ',' 'const' T_VARIABLE				
	|	non_empty_parameter_list ',' T_VARIABLE '=' static_scalar 	
;

function_call_parameter_list:
		non_empty_function_call_parameter_list	
	|	/* empty */				
;

non_empty_function_call_parameter_list:
		expr_without_variable	
	|	cvar					
	|	'&' w_cvar 				
	|	non_empty_function_call_parameter_list ',' expr_without_variable	
	|	non_empty_function_call_parameter_list ',' cvar						
	|	non_empty_function_call_parameter_list ',' '&' w_cvar				
;

global_var_list:
		global_var_list ',' global_var	
	|	global_var						
;

global_var:
		T_VARIABLE			
	|	'$' r_cvar			
	|	'$' '{' expr '}'	
;

static_var_list:
		static_var_list ',' T_VARIABLE 
	|	static_var_list ',' T_VARIABLE '=' static_scalar 
	|	T_VARIABLE  
	|	T_VARIABLE '=' static_scalar 

;

class_statement_list:
		class_statement_list class_statement
	|	/* empty */
;

class_statement:
		'var' class_variable_decleration ';'
	|	'function'  is_reference T_STRING  '(' 
			parameter_list ')' '{' inner_statement_list '}' 
	|	'old_function'  is_reference T_STRING 
			parameter_list '(' inner_statement_list ')' ';' 

;

is_reference:
		/* empty */	
	|	'&'			
;

class_variable_decleration:
		class_variable_decleration ',' T_VARIABLE					
	|	class_variable_decleration ',' T_VARIABLE '=' static_scalar	
	|	T_VARIABLE						
	|	T_VARIABLE '=' static_scalar	
;

echo_expr_list:	
	|	echo_expr_list ',' expr 
	|	expr					
;

for_expr:
		/* empty */			
	|	non_empty_for_expr	
;

non_empty_for_expr:
		non_empty_for_expr ','	 expr 
	|	expr					
;

expr_without_variable:	
		'list' '('  assignment_list ')' '=' expr 
	|	cvar '=' expr		
	|	cvar '=' '&' w_cvar	
	|	cvar '=' '&' function_call 
	|	cvar '=' '&' 'new' static_or_variable_string  ctor_arguments 
	|	'new' static_or_variable_string  ctor_arguments 
	|	cvar '+=' expr 	
	|	cvar '-=' expr	
	|	cvar '*=' expr		
	|	cvar '/=' expr		
	|	cvar '.=' expr	
	|	cvar '%=' expr		
	|	cvar '&=' expr		
	|	cvar '|=' expr 		
	|	cvar '^=' expr 		
	|	cvar '<<=' expr	 
	|	cvar '>>=' expr	 
	|	rw_cvar '++'
	|	'++' rw_cvar
	|	rw_cvar '--'
	|	'--' rw_cvar
	|	expr '||' expr
	|	expr '&&' expr   
	|	expr 'or' expr 
	|	expr 'and' expr 
	|	expr 'xor' expr 
	|	expr '|' expr	
	|	expr '&' expr	
	|	expr '^' expr	
	|	expr '.' expr 	
	|	expr '+' expr 	
	|	expr '-' expr 	
	|	expr '*' expr	
	|	expr '/' expr	
	|	expr '%' expr 	
	| 	expr '<<' expr	
	|	expr '>>' expr	
	|	'+' expr 
	|	'-' expr 
	|	'!' expr 
	|	'~' expr 
	|	expr '===' expr		
	|	expr '!==' expr	
	|	expr '==' expr			
	|	expr '!=' expr 		
	|	expr '<' expr 					
	|	expr '<=' expr 
	|	expr '>' expr 					
	|	expr '>=' expr 
	|	'(' expr ')' 	
	|	expr '?' expr ':' expr	 
	|	function_call 
	|	internal_functions_in_yacc 
	|	'(' 'int' ')' expr 	
	|	'(' 'integer' ')' expr 	
	|	'(' 'double' ')' expr 	
	|	'(' 'string' ')' expr	 
	|	'(' 'array' ')' expr 	
	|	'(' 'object' ')' expr 	
	|	'(' 'bool' ')' expr	
	|	'(' 'boolean' ')' expr	
	|	'(' 'unset' ')' expr	
	|	'exit' exit_expr	
	|	'@'  expr 
	|	scalar				
	|	'array' '(' array_pair_list ')' 
	|	'`' encaps_list '`'		
	|	'print' expr  
;

function_call:
		T_STRING	'(' 
				function_call_parameter_list
				')' 
	|	cvar '('  
				function_call_parameter_list 
				')' 
	|	T_STRING '::' static_or_variable_string '('  
											function_call_parameter_list 
											')' 
;

static_or_variable_string:
		T_STRING	
	|	r_cvar		
;

exit_expr:
		/* empty */		
	|	'(' ')'			
	|	'(' expr ')'	
;

ctor_arguments:
		/* empty */	
	|	'(' function_call_parameter_list ')'	
;

common_scalar:
		T_LNUMBER 					
	|	T_DNUMBER 					
	|	T_CONSTANT_ENCAPSED_STRING	
	|	T_LINE 						
	|	T_FILE 						
	|	T_CLASS_C					
	|	T_FUNC_C					
;

static_scalar: /* compile-time evaluated scalars */
		common_scalar		
	|	T_STRING 		
	|	'+' static_scalar	
	|	'-' static_scalar	
	|	'array' '(' static_array_pair_list ')' 
;

scalar:
		T_STRING 				

    // This is used in ${ <-- there to get a varname
    // ST_LOOKING_FOR_VARNAME is pushed in the scanner state
	// |	T_STRING_VARNAME

	|	common_scalar			
	|	'"' encaps_list '"' 	
	|	'\'' encaps_list '\''	

    // Not implementing this just yet
	//|	T_START_HEREDOC encaps_list T_END_HEREDOC 
;

static_array_pair_list:
		/* empty */ 
	|	non_empty_static_array_pair_list possible_comma	
;

possible_comma:
		/* empty */
	|	','
;

non_empty_static_array_pair_list:
		non_empty_static_array_pair_list ',' static_scalar '=>' static_scalar	
	|	non_empty_static_array_pair_list ',' static_scalar 
	|	static_scalar '=>' static_scalar 
	|	static_scalar 
;

expr:
		r_cvar					
	|	expr_without_variable	
;

r_cvar:
	cvar 
;

w_cvar:
	cvar 
;

rw_cvar:
	cvar 
;

cvar:
		cvar_without_objects 
	|	cvar_without_objects '->'  ref_list 
;

cvar_without_objects:
		reference_variable 
	|	simple_indirect_reference reference_variable 
;

reference_variable:
		reference_variable '[' dim_offset ']'	
	|	reference_variable '{' expr '}'		
	|	compound_variable			
;
	
compound_variable:
		T_VARIABLE			
	|	'$' '{' expr '}'	
;

dim_offset:
		/* empty */		
	|	expr			
;

ref_list:
		object_property  
	|	ref_list '->'  object_property 
;

object_property:
		object_dim_list 
	|	cvar_without_objects  
;

object_dim_list:
		object_dim_list '[' dim_offset ']'	
	|	object_dim_list '{' expr '}'		
	|	variable_name 
;

variable_name:
		T_STRING		
	|	'{' expr '}'	
;

simple_indirect_reference:
		'$' 
	|	simple_indirect_reference '$' 
;

assignment_list:
		assignment_list ',' assignment_list_element
	|	assignment_list_element
;

assignment_list_element:
		cvar
	|	'list' '('  assignment_list ')'	
	|	/* empty */
;

array_pair_list:
		/* empty */
	|	non_empty_array_pair_list possible_comma
;

non_empty_array_pair_list:
		non_empty_array_pair_list ',' expr '=>' expr
	|	non_empty_array_pair_list ',' expr
	|	expr '=>' expr
	|	expr
	|	non_empty_array_pair_list ',' expr '=>' '&' w_cvar
	|	non_empty_array_pair_list ',' '&' w_cvar
	|	expr '=>' '&' w_cvar
	|	'&' w_cvar
;

encaps_list:
		encaps_list encaps_var 
	|	encaps_list T_STRING			
	|	encaps_list T_NUM_STRING		
	|	encaps_list T_ENCAPSED_AND_WHITESPACE
	|	encaps_list T_CHARACTER
	|	encaps_list T_BAD_CHARACTER
	|	encaps_list '['
	|	encaps_list ']'
	|	encaps_list '{'
	|	encaps_list '}'
	|	encaps_list '->'
	|	/* empty */
;

encaps_var:
		T_VARIABLE 
	|	T_VARIABLE '['  encaps_var_offset ']'	
	|	T_VARIABLE '->' T_STRING 
	|	'${' expr '}' 
	//|	'${' T_STRING_VARNAME '[' expr ']' '}' 
	|	'${' T_STRING '[' expr ']' '}' 
	|	'{' cvar '}' 
;

encaps_var_offset:
		T_STRING		
	|	T_NUM_STRING	
	|	T_VARIABLE		
;

internal_functions_in_yacc:
		'isset' '(' isset_variables ')' 
	|	'empty' '(' cvar ')'	
	|	'include' expr 			
	|	'include_once' expr 	
	|	'eval' '(' expr ')' 	
	|	'require' expr			
	|	'require' expr	
;

isset_variables:
		cvar 				
	|	isset_variables ',' cvar 
;	

whitespace: 
        (blank | comment)*
;

comment: 
        '//' "[^\n]*\n" 
    |   '/*' "[^*]*[^/]?" '*/'
;

blank: 
        "[ \t\r\n]"
;

T_INLINE_HTML:
        '<?php'
        "[^?]*[^>]?"
        '?>'
;

T_CONSTANT_ENCAPSED_STRING:
        '"' "[^$\"]" '"'
;

T_STRING:
        "[a-zA-Z0-9_]"
;

T_VARIABLE:
        '$' T_STRING
;

T_LNUMBER:
        "[0-9]"
;

T_DNUMBER:
        "[0-9]+\.?[0-9]*"
    |   "[0-9]*\.?[0-9]+"
;

T_LINE:
        '__LINE__'
;
T_FILE:
        '__FILE__'
;
T_CLASS_C:
        '__CLASS__'
;
T_FUNC_C:
        '__FUNCTION__'
;
T_NUM_STRING:
        "[0-9]+"
    |   "0x[0-9a-fA-F]+"
;
T_ENCAPSED_AND_WHITESPACE:
        "[\n\t\r #'.:;,()|^&+-/*=%!~<>?@]+"
;
T_CHARACTER:
        "\$[^a-zA-Z_\x7f-\xff{]"
;
T_BAD_CHARACTER:
        "\\[^ntr\\${]"
;

/* {{{
%left 'include' 'include_once' T_EVAL T_REQUIRE T_REQUIRE_ONCE
%left ','
%left T_LOGICAL_OR
%left T_LOGICAL_XOR
%left T_LOGICAL_AND
%right T_PRINT
%left '=' T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
%left '?' ':'
%left T_BOOLEAN_OR
%left T_BOOLEAN_AND
%left '|'
%left '^'
%left '&'
%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL
%nonassoc '<' T_IS_SMALLER_OR_EQUAL '>' T_IS_GREATER_OR_EQUAL
%left T_SL T_SR
%left '+' '-' '.'
%left '*' '/' '%'
%right '!' '~' T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST '@'
%right '['
%nonassoc T_NEW
%left T_ELSEIF
%left T_ELSE
%left T_ENDIF
}}} */


