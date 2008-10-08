
from compiler.ast import *

nodes = {
    '+'           :  Add,
    'and'         :  And,
    'assert'      :  Assert,
    # AssAttr, AssList, AssTuple, Asign, AugAssign
    # Backquote, 
    '&'           : BitAnd,
    '|'           : BitOr,
    '^'           : BitXor,
     'class'       : Class,
    '<'           : Compare,
    '>'           : Compare,
    '/'           : Div,
    
    
    # const, other comparisons
    # Ellipsis
    # Exec
    # FloorDiv
    # For
    # From
    
    'def'         : Function,
    'continue'    : Continue,
    'break'       : Break,
    '@'           : Decorators,
    # dict
    
    
}
