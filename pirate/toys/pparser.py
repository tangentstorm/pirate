"""
pparser.py
2002-02-04
    Fixed parsing bugs in simple_stmt and >> handling
    
2002-02-03

A hand-coded python 2.3 parser that acts as a replacement for the
existing parser module, which is just a wrapper around the C functions.

To attach to the compiler package, save this file in Lib/compiler and change
the 'import parser' statement to 'import compiler.pparser as parser'.  To use
Dragon Book terminology, this module generates a parse tree, and Transformer
creates an AST tree. A totally unnecessary diff for compiler.Transformer.py
is provided at the end of this docstring if you need it.

Notes:

    Couldn't use the tokenizer module directly as it doesn't replicate the real
    tokenizer exactly (for perfectly valid reasons, there's documentation in
    tokenizer.py

    BAD GRAMMAR statements strewn throughout docstrings simply indicate that
    the grammar is not LL(1) as written, and not an indictment of python
    developers ;)  So control flow of these functions won't follow the grammar
    exactly.

Todo:

    Don't need a tokenBuffer, since I'm not rolling back anywhere.  I should
    only need to keep the current token.

    Relocate test code into another file.close

    Write a Win32 friendly version of Tools/Compiler/regrtest.py

Index: transformer.py
===================================================================
RCS file: /cvsroot/python/python/dist/src/Lib/compiler/transformer.py,v
retrieving revision 1.36
diff -c -r1.36 transformer.py
*** transformer.py	3 Jan 2003 10:25:20 -0000	1.36
--- transformer.py	3 Feb 2003 22:24:02 -0000
***************
*** 23,29 ****
  # and replace OWNER, ORGANIZATION, and YEAR as appropriate.
  
  from ast import *
! import parser
  # Care must be taken to use only symbols and tokens defined in Python
  # 1.5.2 for code branches executed in 1.5.2
  import symbol
--- 23,29 ----
  # and replace OWNER, ORGANIZATION, and YEAR as appropriate.
  
  from ast import *
! import compiler.pparser as parser
  # Care must be taken to use only symbols and tokens defined in Python
  # 1.5.2 for code branches executed in 1.5.2
  import symbol

"""
import token,symbol, tokenize

literals = {} # token doesn't return proper IDs for keywords

def _addL(name, Type):
    global literals
    literals[name] = getattr(token, Type)
    
_addL('def','NAME')
_addL('print', 'NAME')
_addL('del', 'NAME')
_addL('pass', 'NAME')
_addL('break', 'NAME')
_addL('try', 'NAME')
_addL('except', 'NAME')
_addL('finally', 'NAME')
_addL('continue','NAME')
_addL('return', 'NAME')
_addL('class', 'NAME')
_addL('yield', 'NAME')
_addL('raise', 'NAME')
_addL('import', 'NAME')
_addL('from', 'NAME')
_addL('global', 'NAME')
_addL('exec', 'NAME')
_addL('in', 'NAME')
_addL('assert', 'NAME')
_addL('if', 'NAME')
_addL('elif', 'NAME')
_addL('else','NAME')
_addL('while', 'NAME')
_addL('continue', 'NAME')
_addL('for', 'NAME')
_addL('or', 'NAME')
_addL('and', 'NAME')
_addL('not', 'NAME')
_addL('is', 'NAME')
_addL('lambda', 'NAME')

_addL(':', 'COLON')
_addL('(', 'LPAR')
_addL(')', 'RPAR')
_addL('[', 'LSQB')
_addL(']', 'RSQB')
_addL(',', 'COMMA')
_addL(';', 'SEMI')
_addL('+', 'PLUS')
_addL('-', 'MINUS')
_addL('*', 'STAR')
_addL('/', 'SLASH')
_addL('|', 'VBAR')
_addL('&', 'AMPER')
_addL('<', 'LESS')
_addL('>', 'GREATER')
_addL('=', 'EQUAL')
_addL('.', 'DOT')
_addL('%', 'PERCENT')
_addL('`', 'BACKQUOTE')
_addL('{', 'LBRACE')
_addL('}', 'RBRACE')
_addL('==', 'EQEQUAL')
_addL('!=', 'NOTEQUAL')
_addL('<>', 'NOTEQUAL')
_addL('<=', 'LESSEQUAL')
_addL('>=','GREATEREQUAL' )
_addL('~', 'TILDE')
_addL('^', 'CIRCUMFLEX')
_addL('<<', 'LEFTSHIFT')
_addL('>>', 'RIGHTSHIFT')
_addL('**', 'DOUBLESTAR')
_addL('+=', 'PLUSEQUAL')
_addL('-=', 'MINEQUAL')
_addL('*=', 'STAREQUAL')
_addL('/=', 'SLASHEQUAL')
_addL('%=', 'PERCENTEQUAL')
_addL('&=', 'AMPEREQUAL')
_addL('|=', 'VBAREQUAL')
_addL('^=', 'CIRCUMFLEXEQUAL')
_addL('<<=', 'LEFTSHIFTEQUAL')
_addL('>>=', 'RIGHTSHIFTEQUAL')
_addL('**=', 'DOUBLESTAREQUAL')
_addL('//', 'DOUBLESLASH')
_addL('//=', 'DOUBLESLASHEQUAL')


if 1:
    import sys
    debugPrint = sys.stdout.write
else:
    def debugPrint(x): pass
    
class tokenBuffer:
    """
    Keeps track of our stream of tokens in case we need to roll back
    """
    def __init__(self, tokenGen):
        self.tokenGen = tokenGen
        self.tokens = []
        self.currentPosition = -1
        self.topPosition = -1 #length of tokens array
        self.tokensConsumed = 0
        self.consumeToken()

    def currentToken(self):
        return self.tokens[self.currentPosition]
    def tokType(self):
        return self.tokens[self.currentPosition][0]
    
    def tokVal(self):
        return self.tokens[self.currentPosition][1]
    def tokLine(self):
        return self.tokens[self.currentPosition][3][0]
    def consumeToken(self):
        " We decided the current token has been used, get the next one"
        self.currentPosition += 1
        if self.currentPosition >= self.topPosition:
            self.tokens.append(self.getNextToken())
            self.topPosition += 1
        
    def getNextToken(self):
        "trys to grab a token, uses the original stream if necessary"
        try:
            tok = self.tokenGen.next()
            #blank and comment only lines are WHITESPACE not tokens, per tokenize.c
            while tok[0] == 52 or tok[0] == 53:
                tok = self.tokenGen.next()
            self.tokensConsumed += 1
            return tok
        except StopIteration:
            return (0, '', (0,0), (0,0), '<EOF>')

class parser:
    def __init__(self, tb):
        self.tb = tb #tokenBuffer object

    def required(self, obj, msg):
        if not obj:
            tok = self.tb.currentToken()
            pos = " " * tok[2][1]
            errorMessage = "%s\nLINE %s:\n%s\n" % (msg, tok[2][0], tok[4])
            errorMessage += pos + "^^^"
            raise SyntaxError(errorMessage)
    
    #
    # TOKENS
    #
       
    def tokenLITERAL(self, string):
        "matches input exactly"
        if self.tb.tokVal() == string:
            ret = (literals[self.tb.tokVal()], self.tb.tokVal(), self.tb.tokLine())
            self.tb.consumeToken()
            return ret

    def token(self, tokType):
        if tokType == token.NAME and literals.has_key(self.tb.tokVal()): return None #Invalid keyword Syntax Error?
        if self.tb.tokType() == tokType:
            ret = (self.tb.tokType(), self.tb.tokVal(), self.tb.tokLine())
            self.tb.consumeToken()
            return ret
        
    def tokenNAME(self):
        name = self.token(token.NAME)
        if name and literals.has_key(name[1]): raise SyntaxError("Invalid Keyword")
        return name
    def tokenNUMBER(self):return self.token(token.NUMBER)
    def tokenSTRING(self):return self.token(token.STRING)      
    def tokenNEWLINE(self):return self.token(token.NEWLINE)
    def tokenENDMARKER(self):return self.token(token.ENDMARKER)
    def tokenINDENT(self):return self.token(token.INDENT)
    def tokenDEDENT(self):return self.token(token.DEDENT)
      
    
    #
    # SYMBOLS
    #
    def single_input(self):
        """
        NEWLINE |
        simple_stmt |
        compound_stmt NEWLINE
        """
        tok = self.tokenNEWLINE()
        if tok: return (symbol.single_input, tok)
        sym = self.simple_stmt()
        if sym: return sym
        sym = self.compound_stmt()
        if sym:
            tok = self.tokenNEWLINE()
            if tok:
                return (symbol.single_input, sym, tok)
            else:
                raise "ERROR IN single_input (compound_stmt NEWLINE)"
    
    def file_input(self):
        """
        (NEWLINE | stmt)* ENDMARKER
        """
        def newlineOrStatement():
            ret = []
            while 1:
                tok = self.tokenNEWLINE()
                if tok:
                    ret.append(tok)
                else:
                    sym = self.stmt()
                    if sym:
                        ret.append(sym)
                    else:
                        return ret
        nos = newlineOrStatement()
        endmarker = self.tokenENDMARKER()
        self.required(endmarker, "INVALID FILE INPUT, NO ENDMARKER")
        node = [symbol.file_input]
        if nos: node.extend(nos)
        node.append( (4,'') ) # hack to match real parsers output
        node.append(endmarker)
        return tuple(node)
    
    def eval_input(self):
        """
        testlist NEWLINE* ENDMARKER
        """
        def newlines():
            ret = []
            while 1:
                tok = self.tokenNEWLINE()
                if tok:
                    ret.append(tok)
                else:
                    return ret
        testlist = self.testlist()
        if not testlist: return None
        nls = newlines()
        em = self.tokenENDMARKER()
        self.required(em, "INVALID eval_input, NO ENDMARKER")
        node = [symbol.eval_input]
        node.append(testlist)
        node.extend(newlines)
        node.append(em)
        return tuple(node)

    def funcdef(self):
        """
        'def' NAME parameters ':' suite
        """
        _def = self.tokenLITERAL('def')
        if not _def: return None
        
        funcName = self.tokenNAME()
        self.required(funcName, "INVALID FUNCTION PROTOTYPE, BAD NAME")
        params = self.parameters()
        self.required(params, "INVALID FUNCTION PROTOTYPE, NO PARAMETERS")
        colon = self.tokenLITERAL(':')
        self.required(colon,"INVALID FUNCTION PROTOTYPE, NO COLON")
        suite = self.suite()
        self.required(suite,"INVALID FUNCTION PROTOTYPE, NO SUITE")
        return (symbol.funcdef, _def, funcName, params, colon, suite)
    
    def parameters(self):
        """'(' [varargslist] ')'"""
        openBrace = self.tokenLITERAL('(')
        if not openBrace: return None
        varargs = self.varargslist()
        closeBrace = self.tokenLITERAL(')')
        self.required(closeBrace, "INVALID  PARAMETER LIST, NO CLOSING BRACE ')'")
        if varargs:
            return (symbol.parameters, openBrace, varargs, closeBrace)
        else:
            return (symbol.parameters, openBrace, closeBrace)
    
    def varargslist(self):
        """
        (fpdef ['=' test] ',')* ('*' NAME [',' '**' NAME] | '**' NAME) |
        fpdef ['=' test] (',' fpdef ['=' test])* [',']
        BAD GRAMMAR        """
        testDefs = []
        while 1:
            fpdef = self.fpdef()
            if not fpdef: break
            testDefs.append(fpdef)
            equal = self.tokenLITERAL('=')
            if equal:
                test = self.test()
                self.required(test, "INVALID VARARGSLIST, KEYWORD WASN'T ASSIGNED ANYTHING")
                testDefs.append(equal)
                testDefs.append(test)
            comma = self.tokenLITERAL(',')
            if not comma: break
            testDefs.append(comma)
        starDefs = []
        if not testDefs or testDefs[-1][1] == ',': #may have * arg
            star = self.tokenLITERAL('*')
            if star:
                starName = self.tokenNAME()
                self.required(starName, "INVALID varargslist, NOTHING AFTER *")
                starDefs.append(star)
                starDefs.append(starName)
                starComma = self.tokenLITERAL(',')
                if starComma: starDefs.append(starComma)
        starStarDefs = []
        if (starDefs and starDefs[-1][1] == ',') or (not starDefs and (not testDefs or testDefs[-1][1] == ',')):
            starStar = self.tokenLITERAL("**")
            if starStar:
                starStarName = self.tokenNAME()
                self.required(starStarName, "Invalid varargs, no variable name for **")
                starStarDefs.append(starStar)
                starStarDefs.append(starStarName)
                comma = self.tokenLITERAL(',')
                if comma: starStarDefs.append(comma)
        if not (testDefs or starDefs or starStarDefs): return None
        node = [symbol.varargslist]
        if testDefs: node.extend(testDefs)
        if starDefs: node.extend(starDefs)
        if starStarDefs: node.extend(starStarDefs)
        return tuple(node)
    def fpdef(self):
        """
        NAME |
        '(' fplist ')'
        """
        name = self.tokenNAME()
        if name: return (symbol.fpdef, name)
        openBrace = self.tokenLITERAL('(')
        if not openBrace: return None
        fplist = self.fplist()
        self.required(fplist, "INVALID FPDEF, NO fplist")
        closeBrace = self.tokenLITERAL(')')
        self.required(closeBrace, "INVALID FPDEF, NO CLOSING BRACE")
        return (symbol.fpdef, openBrace, fplist, closeBrace)
    
    def fplist(self):
        """
        fpdef (',' fpdef)* [',']
        """
        def trailingDefs():
            ret = []
            while 1:
                tok = self.tokenLITERAL(',')
                if tok:
                    sym = self.fpdef()
                    self.required(sym, "INVALIDE FPLIST")
                    ret.append(tok)
                    ret.append(sym)
                else:
                    break
            return ret
        fpFirst = self.fpdef()
        if not fpFirst: return
        fpRest = trailingDefs()
        optionalComma = self.tokenLITERAL(",")
        node = [symbol.fplist]
        node.append(fpFirst)
        node.extend(fpRest)
        if optionalComma:node.append(optionalComma)
        return tuple(node)

    def stmt(self):
        """
        simple_stmt |
        compound_stmt
        """
        sym = self.simple_stmt()
        if sym: return (symbol.stmt, sym)
        sym = self.compound_stmt()
        if sym: return (symbol.stmt, sym)
    
    def simple_stmt(self):
        """
         (';' small_stmt)* [';'] NEWLINE
        """
        def multipleStatements():
            ret = []
            while 1:
                tok = self.tokenLITERAL(";")
                if not tok: break
                ret.append(tok)
                sym = self.small_stmt()
                if not sym: break
                ret.append(sym)
            return ret
        smallFirst = self.small_stmt()
        if not smallFirst: return None
        smallRest = multipleStatements()
        optionalSemi = self.tokenLITERAL(";")
        nl = self.tokenNEWLINE()
        self.required(nl, "INVALID SYNTAX simple_statement:: newline Required")
        node = [symbol.simple_stmt]
        node.append(smallFirst)
        node.extend(smallRest)
        if optionalSemi: node.append(optionalSemi)
        node.append(nl)
        return tuple(node)
    
    def small_stmt(self):
        """
        expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | exec_stmt |
        assert_stmt
        """
        sym = self.expr_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.print_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.del_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.pass_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.flow_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.import_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.global_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.exec_stmt()
        if sym: return (symbol.small_stmt, sym)
        sym = self.assert_stmt()
        if sym: return (symbol.small_stmt, sym)
        return None
    
    def expr_stmt(self):
        """
        testlist (augassign testlist | ('=' testlist)*)
        """
        def augassignTestlist():
            augassign = self.augassign()
            if not augassign: return None
            tl2 = self.testlist()
            self.required(tl2, "INVALID EXPR_STMT, augtestlist")
            return [augassign, tl2]
        def equalsTestlist():
            ret = []
            while 1:
                equals = self.tokenLITERAL('=')
                if not equals: return ret
                tl2 = self.testlist()
                self.required(tl2, "INVALID EXPR_STMT, equalsTestList")
                ret.append(equals)
                ret.append(tl2)
        tl = self.testlist()
        if not tl: return
        aug = augassignTestlist()
        node = [symbol.expr_stmt]
        if aug:
            node.append(tl)
            node.extend(aug)
            return tuple(node)
        else:
            eq = equalsTestlist()
            if eq:
                node.append(tl)
                node.extend(eq)
                return tuple(node)
            else:
                return (symbol.expr_stmt, tl)
    
    def augassign(self):
        """
        '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '**=' | '//='
        """
        tok = self.tokenLITERAL('+=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('-=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('*=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('/=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('%=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('&=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('|=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('^=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('<<=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('>>=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('**=')
        if tok: return symbol.augassign, tok
        tok = self.tokenLITERAL('//=')
        if tok: return symbol.augassign, tok
        return None
    
# For normal assignments, additional restrictions enforced by the interpreter"""
    def print_stmt(self):
        """
        'print' ( [ test (',' test)* [','] ] | '>>' test [ (',' test)+ [','] ] )
        BAD GRAMMAR
        """
        def standardPrint():
            test1 = self.test()
            if not test1: return None
            tail = [test1]
            while 1:
                comma = self.tokenLITERAL(',')
                if not comma: break
                tail.append(comma)
                test2 = self.test()
                if not test2: break
                tail.append(test2)
            return tail
        def pipedPrint():
            pipe = self.tokenLITERAL('>>')
            if not pipe: return None
            test1 = self.test()
            self.required(test1, "INVALID PRINT STATMENT, PIPING TO NOTHING")
            tail = [pipe, test1]
            while 1:
                comma = self.tokenLITERAL(',')
                if not comma: break
                test2 = self.test()
                if not test2: break
                tail.append(comma)
                tail.append(test2)
            return tail
        print_ = self.tokenLITERAL('print')
        if not print_: return None
        tail = standardPrint()
        if not tail:
            tail = pipedPrint()
            if not tail: return (symbol.print_stmt, print_)
        node = [symbol.print_stmt, print_]
        node.extend(tail)
        return tuple(node)
    
    def del_stmt(self):
        """
        'del' exprlist
        """
        _del = self.tokenLITERAL('del')
        if not _del: return None
        exprlist = self.exprlist()
        self.required(exprlist, "INVALID DEL; NO EXPRLIST")
        return (symbol.del_stmt, _del, exprlist)
    
    def pass_stmt(self):
        """
        'pass'
        """
        _pass = self.tokenLITERAL('pass')
        if _pass:
            return (symbol.pass_stmt, _pass)
    
    def flow_stmt(self):
        """
        break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
        """
        sym = self.break_stmt()
        if sym: return (symbol.flow_stmt, sym)
        sym = self.continue_stmt()
        if sym: return (symbol.flow_stmt, sym)
        sym = self.return_stmt()
        if sym: return (symbol.flow_stmt, sym)
        sym = self.raise_stmt()
        if sym: return (symbol.flow_stmt, sym)
        sym = self.yield_stmt()
        if sym: return (symbol.flow_stmt, sym)
        return None
        
    def break_stmt(self):
        """
        'break'
        """
        tok = self.tokenLITERAL('break')
        if tok: return (symbol.break_stmt, tok)

    def continue_stmt(self):
        """
        'continue'
        """
        tok = self.tokenLITERAL('continue')
        if tok: return (symbol.continue_stmt, tok)
    
    def return_stmt(self):
        """
        'return' [testlist]
        """
        ret = self.tokenLITERAL('return')
        if not ret: return
        tl = self.testlist()
        if not tl:
            return (symbol.return_stmt, ret)
        else:
            return (symbol.return_stmt, ret, tl)
    
    def yield_stmt(self):
        """
        'yield' testlist
        """
        _yield = self.tokenLITERAL('yield')
        if not _yield: return None
        testlist = self.testlist()
        self.required(testlist, "INVALID YIELD, REQUIRES TESTLIST")
        return (symbol.yield_stmt, _yield, testlist)
    
    def raise_stmt(self):
        """
        'raise' [test [',' test [',' test]]]
        """
        _raise = self.tokenLITERAL('raise')
        if not _raise: return None
        test1 = self.test()
        if not test1: return (symbol.raise_stmt, _raise)
        comma1 = self.tokenLITERAL(',')
        if not comma1: return (symbol.raise_stmt, _raise, test1)
        test2 = self.test()
        self.required(test2, "INVALID RAISE, missing test2")
        comma2 = self.tokenLITERAL(',')
        if not comma2: return (symbol.raise_stmt, _raise, test1, comma1, test2)
        test3 = self.test()
        self.required(test3, "INVALID RAISE, missing test3")
        return (symbol.raise_stmt, _raise, test1, comma1, test2, comma2, test3)
    
    def import_stmt(self):
        """
        'import' dotted_as_name (',' dotted_as_name)* |
        'from' dotted_name 'import' ('*' | import_as_name (',' import_as_name)*)
        """
        def importAs():
            import_ = self.tokenLITERAL('import')
            if not import_: return None
            dottedAsName = self.dotted_as_name()
            self.required(dottedAsName, "INVALID IMPORT, NOT IMPORTING ANYTHING")
            optionalNames = []
            while 1:
                comma = self.tokenLITERAL(',')
                if not comma: break
                dottedAsName2 = self.dotted_as_name()
                self.required(dottedAsName2, "INVALID IMPORT, TRAILING COMMA")
                optionalNames.append(comma)
                optionalNames.append(dottedAsName2)
            return [import_, dottedAsName] + optionalNames
        def fromImport():
            from_ = self.tokenLITERAL('from')
            if not from_: return None
            dotted_name = self.dotted_name()
            self.required(dotted_name, "INVALID FROM..IMPORT STATEMENT, NO MODULE")
            import_ = self.tokenLITERAL('import')
            self.required(import_, "INVALID FROM...IMPORT STATEMENT, NO IMPORT KEYWORD")
            star = self.tokenLITERAL("*")
            if star: return from_, dotted_name, import_, star
            importAsName = self.import_as_name()
            self.required(importAsName, "INVALID FROM...IMPORT STATEMENT, NOT IMPORTING ANYTHING")
            tail = []
            while 1:
                comma = self.tokenLITERAL(',')
                if not comma: break
                importAsName2 = self.import_as_name()
                self.required(importAsName2, "INVALID FROM...IMPORT STATEMENT, TRAILING COMMA")
                tail.append(comma)
                tail.append(importAsName2)
            return [from_, dotted_name, import_, importAsName] + tail
        children = importAs()
        if not children:
            children = fromImport()
            if not children: return None
        node = [symbol.import_stmt]
        node.extend(children)
        return tuple(node)
    
    def import_as_name(self):
        """
        NAME [NAME NAME]
        """
        name1 = self.tokenNAME()
        if not name1: return None
        name2 = self.tokenNAME()
        if not name2: return (symbol.import_as_name, name1)
        name3 = self.tokenNAME()
        self.required(name3, "INVALIDE import_as_name")
        return (symbol.import_as_name, name1, name2, name3)

    def dotted_as_name(self):
        """
        dotted_name [NAME NAME]
        """
        name1 = self.dotted_name()
        if not name1: return None
        name2 = self.tokenNAME()
        if not name2: return (symbol.dotted_as_name, name1)
        name3 = self.tokenNAME()
        self.required(name3, "INVALIDE import_as_name")
        return (symbol.dotted_as_name, name1, name2, name3)

    def dotted_name(self):
        """
        NAME ('.' NAME)*
        """
        name1 = self.tokenNAME()
        if not name1: return None
        dots = []
        while 1:
            dot = self.tokenLITERAL('.')
            if not dot: break
            name2 = self.tokenNAME()
            self.required(name2, "INVALID DOTTED NAME, can't end with DOT")
            dots.append(dot)
            dots.append(name2)
        node = [symbol.dotted_name]
        node.append(name1)
        node.extend(dots)
        return tuple(node) 
    
    def global_stmt(self):
        """
        'global' NAME (',' NAME)*
        """
        gbl = self.tokenLITERAL('global')
        if not gbl: return None
        name1 = self.tokenNAME()
        self.required(name1, "INVALID GLOBAL STATEMENT.  AT LEAST ONE VARIABLE REQUIRED")
        node = [symbol.global_stmt, gbl, name1]
        while 1:
            comma = self.tokenLITERAL(',')
            if not comma: break
            name2 = self.tokenNAME()
            self.required(name2, "INVALID GLOBAL STATEMENT. TRAILING COMMA")
            node.append(comma)
            node.append(name2)
        return tuple(node)
            

    def exec_stmt(self):
        """
        'exec' expr ['in' test [',' test]]
        """
        exec_ = self.tokenLITERAL('exec')
        if not exec_: return None
        expr = self.expr()
        self.required(expr, "INVALID exec, must exec something")
        in_ = self.tokenLITERAL('in')
        if not in_: return (symbol.exec_stmt, exec_, expr)
        test1 = self.test()
        self.required(test1, "INVALID EXEC STATEMENT.  WHAT ARE YOU EXEC'ING IN?")
        comma = self.tokenLITERAL(',')
        if not comma: return (symbol.exec_stmt, exec_, expr, in_, test1)
        test2 = self.test()
        self.required(test1, "INVALID EXEC STATEMENT.  WHAT ARE YOU EXEC'ING IN?")
        return (symbol.exec_stmt, exec_, expr, in_, test1, comma, test2)

    def assert_stmt(self):
        """
        'assert' test [',' test]
        """
        assert_ = self.tokenLITERAL('assert')
        if not assert_: return None
        test1 = self.test()
        self.required(test1, "INVALID ASSERT, WHAT ARE YOU ASSERTING?")
        comma = self.tokenLITERAL(',')
        if not comma: return (symbol.assert_stmt, assert_, test1)
        test2 = self.test()
        self.required(test2, "INVALID ASSERT, NEED AN ERROR MESSAGE")
        return (symbol.assert_stmt, assert_, test1, comma, test2)

    def compound_stmt(self):
        """
        if_stmt | while_stmt | for_stmt | try_stmt | funcdef | classdef
        """
        sym = self.if_stmt()
        if sym: return (symbol.compound_stmt, sym)
        sym = self.while_stmt()
        if sym: return (symbol.compound_stmt, sym)
        sym = self.for_stmt()
        if sym: return (symbol.compound_stmt, sym)
        sym = self.try_stmt()
        if sym: return (symbol.compound_stmt, sym)
        sym = self.funcdef()
        if sym: return (symbol.compound_stmt, sym)
        sym = self.classdef()
        if sym: return (symbol.compound_stmt, sym)
    
    def if_stmt(self):
        """
        'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
        """
        if_ = self.tokenLITERAL('if')
        if not if_: return None
        test = self.test()
        self.required(test, "Invalid IF statment, no test condition")
        colon = self.tokenLITERAL(':')
        self.required(test, "Invalid IF statement, no colon")
        suite = self.suite()
        self.required(suite, "Invalid IF statement, no suite")
        node = [symbol.if_stmt, if_, test, colon, suite]
        elifs = []
        while 1:
            elif_ = self.tokenLITERAL('elif')
            if not elif_: break
            elifTest = self.test()
            self.required(elifTest, "Invalid if statement, ELIF missing test")
            elifColon = self.tokenLITERAL(':')
            self.required(elifColon, "Invalid if statement, no colon in ELIF")
            elifSuite = self.suite()
            self.required(elifSuite, "Invalid if statement, elif performs no actions")
            elifs.append(elif_)
            elifs.append(elifTest)
            elifs.append(elifColon)
            elifs.append(elifSuite)
        if elifs: node.extend(elifs)
        else_ = self.tokenLITERAL('else')
        if not else_: return tuple(node)
        elseColon = self.tokenLITERAL(':')
        self.required(elseColon, "Invalid if statement, else clause missing colon")
        elseSuite = self.suite()
        self.required(elseSuite, "Invalid if statement, else statement does nothing")
        node.append(else_)
        node.append(elseColon)
        node.append(elseSuite)
        return tuple(node)

    def while_stmt(self):
        """
        'while' test ':' suite ['else' ':' suite]
        """
        while_ = self.tokenLITERAL('while')
        if not while_: return None
        test = self.test()
        self.required(test, "Invalid while statement, no test")
        colon = self.tokenLITERAL(':')
        self.required(colon, "Invalid while statement, no colon")
        suite = self.suite()
        self.required(suite, "Invalid while statement, no suite")
        else_ = self.tokenLITERAL('else')
        if not else_: return symbol.while_stmt, while_, test, colon, suite
        elseColon = self.tokenLITERAL(':')
        self.required(elseColon, "Invalid while statement, no colon after else clause")
        elseSuite = self.suite()
        self.required(elseSuite, "Invalid while statement, no suite in else clause")
        return symbol.while_stmt, while_, test, colon, suite, else_, elseColon, elseSuite

    def for_stmt(self):
        """
        'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
        """
        for_ = self.tokenLITERAL('for')
        if not for_: return None
        exprlist = self.exprlist()
        self.required(exprlist, "Invalid for_stmt")
        in_ = self.tokenLITERAL('in')
        self.required(in_, "Invalid for_stmt")
        testlist = self.testlist()
        self.required(testlist, "Invalid for stmt")
        colon = self.tokenLITERAL(':')
        self.required(colon, "invalid for stmt")
        suite = self.suite()
        self.required(suite, "invalid for stmt")
        else_ = self.tokenLITERAL('else')
        if not else_:
            return symbol.for_stmt, for_, exprlist, in_, testlist, colon, suite
        elseColon = self.tokenLITERAL(':')
        self.required(elseColon, "for stmt, else clause")
        elseSuite = self.suite()
        self.required(suite, "for stmt, else clause")
        return symbol.for_stmt, for_, exprlist, in_, testlist, colon, suite, else_, elseColon, elseSuite

    def try_stmt(self):
        """
        ('try' ':' suite (except_clause ':' suite)+ #diagram:break
           ['else' ':' suite] | 'try' ':' suite 'finally' ':' suite)
        """
        def tryExcept():
            excepts = []
            while 1:
                except_clause = self.except_clause()
                if not except_clause: break
                exceptColon = self.tokenLITERAL(":")
                self.required(exceptColon, "INVALID TRY.  NO COLON AFTER ACCEPT")
                exceptSuite = self.suite()
                self.required(exceptColon, "INVALID TRY.  NO EXCEPT SUITE")
                excepts.append(except_clause)
                excepts.append(exceptColon)
                excepts.append(exceptSuite)
            if not excepts: return None
            else_ = self.tokenLITERAL('else')
            if else_:
                excepts.append(else_)
                elseColon = self.tokenLITERAL(':')
                self.required(elseColon, "INVALID TRY.  NO COLON AFTER ELSE STATEMENT")
                elseSuite = self.suite()
                self.required(elseSuite, "INVALID TRY. NO SUITE FOR ELSE STATEMENT")
                excepts.append(elseColon)
                excepts.append(elseSuite)
            return excepts
        def tryFinally():
            finally_ = self.tokenLITERAL('finally')
            if not finally_: return None
            finallyColon = self.tokenLITERAL(':')
            self.required(finallyColon, "INVALID TRY STATEMENT.  NO COLON IN FINALLY CLAUSE")
            finallySuite = self.suite()
            self.required(finallySuite, "INVALID TRY STATEMENT.  FINALLY HAS NO ACTIONS")
            return finally_, finallyColon, finallySuite
        try_ = self.tokenLITERAL('try')
        if not try_: return None
        colon = self.tokenLITERAL(':')
        self.required(colon, "INVALID TRY STATEMENT.  NO COLON")
        suite = self.suite()
        self.required(suite, "INVALID TRY STATEMENT.  NO SUITE")
        tail = tryExcept()
        if not tail:
            tail = tryFinally()
            self.required(tail, "INVALID TRY STATEMENT.  MUST BE EITHER try...except or try...finally")
        node = [symbol.try_stmt, try_, colon, suite]
        node.extend(tail)
        return tuple(node)
        
    

    def except_clause(self):
        """
        # NB compile.c makes sure that the default except clause is last
        'except' [test [',' test]]
        """
        except_ = self.tokenLITERAL('except')
        if not except_: return None
        test1 = self.test()
        if not test1: return symbol.except_clause, except_
        comma = self.tokenLITERAL(',')
        if not comma: return symbol.except_clause, except_, test1
        test2 = self.test()
        self.required(test2, "invalid except clause")
        return symbol.except_clause, except_, test1, comma, test2
    
    def suite(self):
        """
        simple_stmt |
        NEWLINE INDENT stmt+ DEDENT
        """
        simple_stmt = self.simple_stmt()
        if simple_stmt: return (symbol.suite, simple_stmt)
        nl = self.tokenNEWLINE()
        if not nl: return None
        indent = self.tokenINDENT()
        self.required(indent, "INVALID SUITE: Needs indent")
        stmts = []
        while 1:
            sym = self.stmt()
            if not sym: break
            stmts.append(sym)
        self.required(stmts, "INVALID SUITE: Needs at least one statement")
        dedent = self.tokenDEDENT()
        self.required(dedent, "INVALID SUITE: Needs a DEDENT")
        node = [symbol.suite]
        node.append(nl)
        node.append(indent)
        node.extend(stmts)
        node.append(dedent)
        return tuple(node)

    def test(self):
        """
        and_test ('or' and_test)* |
        lambdef
        """
        and_test = self.and_test()
        if not and_test:
            lambdef = self.lambdef()
            if not lambdef:
                return None
            else:
                return symbol.test, lambdef
        tail = []
        while 1:
            tok = self.tokenLITERAL('or')
            if not tok: break
            sym = self.and_test()
            self.required(sym, "INVALID TEST, NEED ANOTHER AND_TEST AFTER 'OR'")
            tail.append(tok)
            tail.append(sym)
        node = [symbol.test]
        node.append(and_test)
        if tail: node.extend(tail)
        return tuple(node)
    
    def and_test(self):
        """
        not_test ('and' not_test)*
        """
        not_test = self.not_test()
        if not not_test: return None
        tail = []
        while 1:
            tok = self.tokenLITERAL('and')
            if not tok: break
            sym = self.not_test()
            self.required(sym, "INVALID and_test, missing trailing not_test")
            tail.append(tok)
            tail.append(sym)
        node = [symbol.and_test]
        node.append(not_test)
        if tail: node.extend(tail)
        return tuple(node)
    
    def not_test(self):
        """
        'not' not_test |
        comparison
        """
        tok = self.tokenLITERAL('not')
        if tok:
            sym = self.not_test()
            self.required(sym, "INVALID not_test, no trailing not_test")
            return (symbol.not_test, tok, sym)
        else:
            sym = self.comparison()
            if not sym:
                return None
            else:
                return (symbol.not_test, sym)
    
    def comparison(self):
        """
        expr (comp_op expr)*
        """
        expr = self.expr()
        if not expr: return None
        tail = []
        while 1:
            comp_op = self.comp_op()
            if not comp_op: break
            expr2 = self.expr()
            self.required(expr2, "INVALID COMPARISON, COMPARING AGAINST NOTHING")
            tail.append(comp_op)
            tail.append(expr2)
        node = [symbol.comparison]
        node.append(expr)
        if tail: node.extend(tail)
        return tuple(node)
    
    def comp_op(self):
        """
        '<' | '>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
        BAD GRAMMAR        """
        tok = self.tokenLITERAL('<')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('>')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('==')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('>=')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('<=')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('<>')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('!=')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('in')
        if tok: return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('not')
        if tok:
            tok2 = self.tokenLITERAL('in')
            if tok2:
                return (symbol.comp_op, tok, tok2)
            else:
                return (symbol.comp_op, tok)
        tok = self.tokenLITERAL('is')
        if tok:
            tok2 = self.tokenLITERAL('not')
            if not tok2:
                return (symbol.comp_op, tok)
            else:
                return (symbol.comp_op, tok, tok2)
            
    def expr(self):
        """
        xor_expr ('|' xor_expr)*
        """
        xor_expr = self.xor_expr()
        if not xor_expr: return None
        tail = []
        while 1:
            tok = self.tokenLITERAL('|')
            if not tok: break
            sym = self.xor_expr()
            self.required(self.xor_expr, "INVALID expr")
            tail.append(tok)
            tail.append(sym)
        node = [symbol.expr]
        node.append(xor_expr)
        if tail: node.extend(tail)
        return tuple(node)

    def xor_expr(self):
        """
        and_expr ('^' and_expr)*
        """
        and_expr = self.and_expr()
        if not and_expr: return and_expr
        tail = []
        while 1:
            tok = self.tokenLITERAL("^")
            if not tok: break
            sym = self.and_expr()
            self.required(sym, "INVALID XOR_EXPR")
            tail.append(tok)
            tail.append(sym)
        node = [symbol.xor_expr, and_expr]
        if tail: node.extend(tail)
        return tuple(node)
    
    def and_expr(self):
        """
        shift_expr ('&' shift_expr)*
        """
        shift_expr = self.shift_expr()
        if not shift_expr: return None
        tail = []
        while 1:
            tok = self.tokenLITERAL('&')
            if not tok: break
            sym = self.shift_expr()
            self.required(sym, "Invalid and_expr, no shift_expr")
            tail.append(tok)
            tail.append(sym)
        node = [symbol.and_expr, shift_expr]
        if tail: node.extend(tail)
        return tuple(node)
    
    def shift_expr(self):
        """
        arith_expr (('<<'|'>>') arith_expr)*
        """
        arith_expr = self.arith_expr()
        if not arith_expr: return None
        tail = []
        while 1:
            tok = self.tokenLITERAL("<<")
            if not tok:
                tok = self.tokenLITERAL(">>")
                if not tok: break
            tail.append(tok)
            sym = self.arith_expr()
            self.required(sym, "INVALID SHIFT_EXPR")
            tail.append(sym)
        node = [symbol.shift_expr, arith_expr]
        if tail: node.extend(tail)
        return tuple(node)
    
    def arith_expr(self):
        """
        term (('+'|'-') term)*
        """
        term = self.term()
        if not term: return None
        tail = []
        while 1:
            tok = self.tokenLITERAL('+')
            if not tok:
                tok = self.tokenLITERAL('-')
                if not tok: break
            sym = self.term()
            self.required(sym, "INVALID ARITH_EXPR")
            tail.append(tok)
            tail.append(sym)
        node = [symbol.arith_expr, term]
        if tail: node.extend(tail)
        return tuple(node)

    def term(self):
        """
        factor (('*'|'/'|'%'|'//') factor)*
        """
        factor = self.factor()
        if not factor: return None
        tail = []
        while 1:
            tok = self.tokenLITERAL('*')
            if not tok:
                tok = self.tokenLITERAL('/')
                if not tok:
                    tok = self.tokenLITERAL('%')
                    if not tok:
                        tok = self.tokenLITERAL('//')
                        if not tok: break
            sym = self.factor()
            self.required(sym, "INVALID TERM")
            tail.append(tok)
            tail.append(sym)
        node = [symbol.term, factor]
        if tail: node.extend(tail)
        return tuple(node)
    
    def factor(self):
        """
        ('+'|'-'|'~') factor |
        power
        """
        tok = self.tokenLITERAL('+')
        if not tok:
            tok = self.tokenLITERAL('-')
            if not tok:
                tok = self.tokenLITERAL('~')
                if not tok:
                    power = self.power()
                    if power:
                        return (symbol.factor, power)
                    else:
                        return None
        fact = self.factor()
        self.required(fact, "INVALID FACTOR")
        return (symbol.factor, tok, fact)

    def power(self):
        """
        atom trailer* ['**' factor]
        """
        atom = self.atom()
        if not atom: return None
        trailers = []
        while 1:
            trailer = self.trailer()
            if not trailer: break
            trailers.append(trailer)
        node = [symbol.power, atom]
        if trailers: node.extend(trailers)
        
        optStarStar = self.tokenLITERAL('**')
        if not optStarStar:
            return tuple(node)
        factor = self.factor()
        self.required(factor, "INVALID power")
        node.append(optStarStar)
        node.append(factor)
        return tuple(node)
    
    def atom(self):
        """
        '(' [testlist] ')' |
        '[' [listmaker] ']' |
        '{' [dictmaker] '}' |
        '`' testlist1 '`' |
        NAME |
        NUMBER |
        STRING+
        """
        def paren():
            open = self.tokenLITERAL('(')
            if not open: return None
            testlist = self.testlist()
            close = self.tokenLITERAL(')')
            self.required(close, "INVALID atom, parens not nested")
            if testlist:
                return open, testlist, close
            else:
                return open, close
        def bracket():
            open = self.tokenLITERAL('[')
            if not open: return None
            listmaker = self.listmaker()
            close = self.tokenLITERAL(']')
            self.required(close, "INVALID atom, parens not nested")
            ret = [open]
            if listmaker:
                return open, listmaker, close
            else:
                return open,close

        def curly():
            open = self.tokenLITERAL('{')
            if not open: return None
            dictmaker = self.dictmaker()
            close = self.tokenLITERAL('}')
            self.required(close, "INVALID atom, parens not nested")
            if dictmaker:
                return open, dictmaker, close
            else:
                return open,close

        def backquote():
            open = self.tokenLITERAL('`')
            if not open: return None
            testlist1 = self.testlist1()
            close = self.tokenLITERAL('`')
            self.required(close, "INVALID atom, parens not nested")
            if testlist1:
                return open, testlist1, close
            else:
                return open,close

        def strings():
            string1 = self.tokenSTRING()
            if not string1: return None
            tail = []
            while 1:
                string2 = self.tokenSTRING()
                if not string2: break
                tail.append(string2)
            return [string1] + tail
        
        node = [symbol.atom]
        tail = paren()
        if tail:
            node.extend(tail)
            return tuple(node)
        tail = bracket()
        if tail:
            node.extend(tail)
            return tuple(node)
        tail = curly()
        if tail:
            node.extend(tail)
            return tuple(node)
        tail = backquote()
        if tail:
            node.extend(tail)
            return tuple(node)
        tok = self.tokenNAME()
        if tok:
            return (symbol.atom, tok)
        tok = self.tokenNUMBER()
        if tok:
            return (symbol.atom, tok)
        tail = strings()
        if tail:
            node.extend(tail)
            return tuple(node)
        return None
        
    
    def listmaker(self):
        """
        test ( list_for | (',' test)* [','] )
        """
        test = self.test()
        if not test: return None
        list_for = self.list_for()
        if list_for:
            return (symbol.listmaker, test, list_for)
        tests = []
        while 1:
            tok = self.tokenLITERAL(',')
            if not tok: break
            tests.append(tok)
            sym = self.test()
            if not sym:break#self.required(sym, "invalid listmaker") # this really needs a rollback
            tests.append(sym)
        node = [symbol.listmaker, test]
        if tests: node.extend(tests)
        return tuple(node)
    
    def lambdef(self):
        """
        'lambda' [varargslist] ':' test
        """
        lambda_ = self.tokenLITERAL('lambda')
        if not lambda_: return None
        optVarargs = self.varargslist()
        colon = self.tokenLITERAL(':')
        self.required(colon, "INVALID LAMBDA, NO COLON")
        test = self.test()
        self.required(test, "INVALID LAMBDA, NO TEST")
        if optVarargs:
            return symbol.lambdef, lambda_, optVarargs, colon, test
        else:
            return symbol.lambdef, lambda_, colon, test

    def trailer(self):
        """
        '(' [arglist] ')' |
        '[' subscriptlist ']' |
        '.' NAME
        """
        def paren():
            openBrace = self.tokenLITERAL('(')
            if not openBrace: return None
            arglist = self.arglist()
            closeBrace = self.tokenLITERAL(')')
            self.required(closeBrace, "Invalid trailer,  needs a closing paren")
            if arglist:
                return openBrace, arglist, closeBrace
            else:
                return openBrace, closeBrace
        def bracket():
            openBrace = self.tokenLITERAL('[')
            if not openBrace: return None
            subscriptlist = self.subscriptlist()
            self.required(subscriptlist, "Invalid trailer, no subscriptlist")
            closeBrace = self.tokenLITERAL(']')
            self.required(closeBrace, "invalid trailer, no closing brace")
            return openBrace, subscriptlist, closeBrace
        def dotName():
            dot = self.tokenLITERAL('.')
            if not dot: return None
            name = self.tokenNAME()
            self.required(name, "Invalid trailer, no name after .")
            return dot, name
        node = [symbol.trailer]
        tail = paren()
        if tail:
            node.extend(tail)
            return tuple(node)
        tail = bracket()
        if tail:
            node.extend(tail)
            return tuple(node)
        tail = dotName()
        if tail:
            node.extend(tail)
            return tuple(node)
    
    def subscriptlist(self):
        """
        subscript (',' subscript)* [',']
        BAD GRAMMAR
        """
        subscript1 = self.subscript()
        if not subscript1: return None
        node = [symbol.subscriptlist, subscript1]
        while 1:
            comma = self.tokenLITERAL(',')
            if not comma: break
            node.append(comma)
            subscript2 = self.subscript()
            if not subscript2: break
            node.append(subscript2)
        return tuple(node)
    
    def subscript(self):
        """
        '.' '.' '.' |
        test |
        [test] ':' [test] [sliceop]
        BAD GRAMMAR
        """
        def elipsis():
            dot1 = self.tokenLITERAL('.')
            if not dot1: return None
            dot2 = self.tokenLITERAL('.')
            self.required(dot2, "INVALID SUBSCRIPT.  ELIPSIS IS ...")
            dot3 = self.tokenLITERAL('.')
            self.required(dot3, "INVALID SUBSCRIPT. ELIPSIS IS ...")
            return dot1, dot2, dot3
        def slice():
            sliceTest = self.test()
            sliceColon = self.tokenLITERAL(':')
            if not (sliceTest or sliceColon): return None
            if sliceTest and not sliceColon: return [sliceTest]
            sliceTest2 = self.test()
            sliceop = self.sliceop()
            ret = []
            if sliceTest: ret.append(sliceTest)
            ret.append(sliceColon)
            if sliceTest2: ret.append(sliceTest2)
            if sliceop: ret.append(sliceop)
            return ret
        
        node = [symbol.subscript]
        tail = elipsis()
        if not tail:
            tail = slice()
            if not tail: return None
        node.extend(tail)
        return tuple(node)
    
    def sliceop(self):
        """
        ':' [test]
        """
        colon = self.tokenLITERAL(':')
        if not colon: return None
        test = self.test()
        if not test:
            return symbol.sliceop, colon
        else:
            return symbol.sliceop, colon, test
    
    def exprlist(self):
        """
        expr (',' expr)* [',']
        BAD GRAMMAR
        """
        expr1 = self.expr()
        if not expr1: return None
        node = [symbol.exprlist, expr1]
        while 1:
            comma = self.tokenLITERAL(',')
            if not comma: break
            node.append(comma)
            expr2 = self.expr()
            if not expr2: break
            node.append(expr2)
        return tuple(node)
    
    def testlist(self):
        """
        test (',' test)* [',']
        BAD GRAMMAR
        """
        test1 = self.test()
        if not test1: return None
        node = [symbol.testlist, test1]
        while 1:
            comma = self.tokenLITERAL(',')
            if not comma: break
            node.append(comma)
            test2 = self.test()
            if not test2: break
            node.append(test2)
        return tuple(node)
    
    def testlist_safe(self):
        """
        test [(',' test)+ [',']]
        """
        test1 = self.test()
        if not test1: return None
        tail = []
        while 1:
            comma = self.tokenLITERAL(',')
            if not comma: break
            tail.append(comma)
            test2 = self.test()
            if not test2: break
            tail.append(test2)
        node = [symbol.testlist_safe, test1] + tail
        return tuple(node)
    
    def dictmaker(self):
        """
        test ':' test (',' test ':' test)* [',']
        """
        test1 = self.test()
        if not test1: return None
        colon1 = self.tokenLITERAL(':')
        self.required(colon1, "INVALID DICTMAKER")
        test2 = self.test()
        self.required(test2, "INVALID DICTMAKER")
        tail = []
        while 1:
            comma = self.tokenLITERAL(',')
            if not comma: break
            tail.append(comma)
            test3 = self.test()
            if not test3: break
            tail.append(test3)
            colon2 = self.tokenLITERAL(':')
            self.required(colon2, "INVALID DICTMAKER")
            tail.append(colon2)
            test4 = self.test()
            self.required(test4, "INVALID DICTMAKER")
            tail.append(test4)
        node = [symbol.dictmaker, test1, colon1, test2]
        if tail: node.extend(tail)
        return tuple(node)

    def classdef(self):
        """
        'class' NAME ['(' testlist ')'] ':' suite
        """
        class_ = self.tokenLITERAL('class')
        if not class_: return
        classname = self.tokenNAME()
        self.required(classname, "INVALID CLASS, REQUIRES A CLASS NAME")
        openBrace = self.tokenLITERAL('(')
        if openBrace:
            testlist = self.testlist()
            self.required(testlist, "INVALID CLASS, NO SUBCLASSES IN PARENS")
            closeBrace = self.tokenLITERAL(")")
            self.required(closeBrace, "INVALID CLASS, NO CLOSING BRACE IN SUBCLASS LIST")
        colon = self.tokenLITERAL(":")
        self.required(colon, "INVALID CLASS, NO TERMINATING COLON")
        suite = self.suite()
        self.required(suite, "INVALID CLASS, NO SUITE")
        if openBrace:
            return symbol.classdef, class_, classname, openBrace, testlist, closeBrace, colon, suite
        else:
            return symbol.classdef, class_, classname, colon, suite
    
    def arglist(self):
        """
        (argument ',')* (argument [',']| '*' test [',' '**' test] | '**' test)
        BAD GRAMMAR
        """
        def args():
            star = self.tokenLITERAL('*')
            if not star: return None
            starTest = self.test()
            self.required(starTest, "INVALID arglist, no varname for *args")
            starComma = self.tokenLITERAL(',')
            if not starComma: return star, starTest
            starStar = self.tokenLITERAL('**')
            self.required(starStar, "Invalid Arglist, invalid trailing comma")
            starStarTest = self.test()
            self.required(starStarTest, "Invalid Arglist, no varname for *kwargs")
            return star, starTest, starComma, starStar, starStarTest
        def kwargs():
            starStar = self.tokenLITERAL('**')
            if not starStar: return None
            starStarTest = self.test()
            self.required(starStarTest, "INVALID arglist, no varname for **kwargs")
            return starStar, starStarTest
        head = [symbol.arglist]
        while 1:
            arg = self.argument()
            if not arg: break
            head.append(arg)
            comma = self.tokenLITERAL(',')
            if not comma:
                return tuple(head)
            head.append(comma)
        tail = args()
        if not tail:
            tail = kwargs()
            if not tail:
                if len(head) <2:
                    return None
                else:
                    return tuple(head) # normal arglist with trailing comma
        head.extend(tail)
        return tuple(head)

    def argument(self):
        """
        [test '='] test	# Really [keyword '='] test
        BAD GRAMMAR
        """
        test1 = self.test()
        if not test1: return None
        eql = self.tokenLITERAL('=')
        if not eql: return symbol.argument, test1
        test2 = self.test()
        self.required(test2, "INVALID ARGUMENT, KEYWORD EQUALS WHAT?")
        return symbol.argument, test1, eql, test2

    def list_iter(self):
        """
        list_for | list_if
        """
        sym = self.list_for()
        if sym: return (symbol.list_iter, sym)
        sym = self.list_if()
        if sym: return (symbol.list_iter, sym)
        return None
    
    def list_for(self):
        """
        'for' exprlist 'in' testlist_safe [list_iter]
        """
        _for = self.tokenLITERAL('for')
        if not _for: return None
        exprlist = self.exprlist()
        self.required(exprlist, "INVALID LIST_FOR, exprlist missing")
        _in = self.tokenLITERAL('in')
        self.required(_in, "INVALID LIST_FOR, needs IN")
        testlist_safe = self.testlist_safe()
        self.required(testlist_safe, "INVALID LIST_FOR, needs testlist_safe")
        list_iter = self.list_iter()
        if list_iter:
            return (symbol.list_for, _for, exprlist, _in, testlist_safe, list_iter)
        else:
            return (symbol.list_for, _for, exprlist, _in, testlist_safe)
    
    def list_if(self):
        """
        'if' test [list_iter]
        """
        iif = self.tokenLITERAL('if')
        if not iif: return None
        test = self.test()
        self.required(test, "INVALID LIST_IF: need a test")
        list_iter = self.list_iter()
        if list_iter:
            return (symbol.list_if, iif, test, list_iter)
        else:
            return (symbol.list_if, iif, test)

    def testlist1(self):
        """
        test (',' test)*
        """
        def tailTests():
            ret = []
            while 1:
                tok = self.tokenLITERAL(',')
                if not tok: return ret
                sym = self.test()
                self.required(sym, "INVALID TESTLIST1")
                ret.append(tok)
                ret.append(sym)
        firstTest = self.test()
        if not firstTest: return None
        restTests = tailTests()
        if not tailTests:
            return 322, firstTest #(symbol.testlist1, firstTest) NOT IN python22
        else:
            node = [322] #[symbol.testlist]
            node.append(firstTest)
            node.extend(restTests)
            return tuple(node)

def stringTG(myString):
    strings = [x + "\n" for x in myString.split('\n')]
    strings.append('')
    return tokenize.generate_tokens(iter(strings).next)

def expr(exprString):
    tb = tokenBuffer(stringTG(exprString))
    ps = parser(tb)
    return ps.eval_input()

def suite(suiteString):
    tb = tokenBuffer(stringTG(suiteString))
    ps = parser(tb)
    return ps.file_input()

def sequence2ast(seq, line_info=0): return seq
def ast2tuple(ast, line_info=0): return ast

if __name__ == "__main__":

    tests = [ ("comp_op", "<\n\n\n\n"),
              ("single_input", "\n"),
              ("file_input", "\n\n\n"),
              ("funcdef", "def foo():pass\n\n"),
              ("funcdef", "def foo():\n    print 'bar'\n\nprint 'bar'\n"),
              ("file_input", "if __name__ == '__main__':\n    def foo():        print 'bar'\n\n\n"),
              ("simple_stmt", "x = 1;y = 2;")
               ]    
    import tokenize

    for test in tests:
        print "TESTING %s ==> %s ::" % (test[0], `test[1]`) ,
        tb = tokenBuffer(stringTG(test[1]))
        ps = parser(tb)
        #print getattr(ps, test[0])()
        if getattr(ps,test[0])():
            print "PASSED"
        else:
            print "FAILED"
        

    #testString = "if __name__ == '__main__':\n    def foo():        print 'bar'\n\n\n"
    #testString = "print 'bar'"
    #tb = tokenBuffer(stringTG(testString))
    #ps = parser(tb)
    #a = ps.file_input()
    #from pprint import pprint
    #pprint(a)

    #import parser as oParser
    #x = oParser.ast2tuple(oParser.suite(testString))
    #pprint(x)

    def execFile(filename):
            print "Execution of %s" % fil ,
            testfile = file(fil)
            tb = tokenBuffer(tokenize.generate_tokens(testfile.readline))
            ps = parser(tb)
            try:
                x = ps.file_input()
                import parser as oParser
                y = oParser.tuple2ast(x)
                z = oParser.compileast(y)
                exec z in globals()
                print "succeeded"
            except:
                print "FAILED"
                
    def parseFile(fil):
        print "Execution of %s" % fil ,
        testfile = file(fil)
        tb = tokenBuffer(tokenize.generate_tokens(testfile.readline))
        ps = parser(tb)
        try:
            x = ps.file_input()
            import parser as oParser
            y = oParser.tuple2ast(x)
            z = oParser.compileast(y)
            print "succeeded"
        except:
            print "FAILED"
            raise

    def parseLib():
        import glob
        files = glob.glob('c:\\python23\\lib\\*.py')
        for fil in files:
            print "Parsing of %s" % fil ,
            testfile = file(fil)
            tb = tokenBuffer(tokenize.generate_tokens(testfile.readline))
            ps = parser(tb)
            try:
                x = ps.file_input()
                import parser as oParser
                y = oParser.tuple2ast(x)
                z = oParser.compileast(y)
                #exec z in globals()
                print "succeeded"
            except:
                print "FAILED"
                #raise

    def dumpTrees(fil):
        from pprint import pprint
        print "Building my Tree for %s" % fil
        testfile = file(fil)
        tb = tokenBuffer(tokenize.generate_tokens(testfile.readline))
        ps = parser(tb)
        x = ps.file_input()
        myTree = file("myTree.txt", "w")
        pprint(x, myTree)
        myTree.close()
        testfile.close()

        print "Building compiler's tree for %s" % fil        
        import parser as oParser
        x = oParser.suite(file(fil).read())
        y = oParser.ast2tuple(x, line_info=1)
        compTree = file("compTree.txt", "w")
        pprint(y, compTree)
        compTree.close()
    #parseFile("c:\\python23\\lib\\textwrap.py")
    #parseLib()
    dumpTrees("c:\\python\\lib\\test\\pickletester.py")
