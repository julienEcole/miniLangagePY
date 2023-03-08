from genereTreeGraphviz2 import printTreeGraph
from pprint import pprint
# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables.
# -----------------------------------------------------------------------------
reserved = {
    'print' : 'PRINT',
    'printstr' : 'PRINTSTR',
    'if' : 'IF',
    'else' : 'ELSE',
    'for' : 'FOR',
    'while' : 'WHILE',
    'function' : 'FUNC',
    'return' : 'RETURN'
   
}

tokens = [
    'NAME','NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','EQUALS',
    'LPAREN','RPAREN', 'SEMI', 'COMA', 'SHARP', 
    'INF','INFEQUAL', 'SUP', 'SUPEQUAL', 'AND', 'OR', 'LACCOLADE', 'RACCOLADE', 'BOOLEQUAL'] + list(reserved.values())
# Tokens


t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_SEMI    = r';'
t_INF     = r'<'
t_INFEQUAL= r'<='
t_SUP     = r'>'
t_SUPEQUAL= r'>='
t_AND     = r'&'
t_LACCOLADE  = r'{'
t_RACCOLADE  = r'}'
t_OR     = r'\|'
t_BOOLEQUAL = r'=='
t_COMA = r'\,'
t_SHARP = r'\#'


def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Ignored characters
t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

def file_vide():
    f = open("printFile.txt", "w")
    f.close()

def file_write(val):
    f = open("printFile.txt", "a") 
    f.write(str(val)+"\n")
    f.close()

# Build the lexer
import ply.lex as lex
lex.lex()

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'INF', 'SUP', 'BOOLEQUAL', 'INFEQUAL', 'SUPEQUAL'),
    ('left','PLUS', 'MINUS'),
    ('left','TIMES','DIVIDE'),
)
 
# dictionary of names (for storing variables)
names = {}
functions={} #{NOM : (PARAMETRE, CORPS)}
isInFunction = False

def evalInst(p):

    if p == 'empty' : return 

    if type(p) != tuple : 
        print("inst non tuple")
        return 

    if p[0] == 'bloc':
        try :
            evalInst(p[1])
            evalInst(p[2])
        except NameError as err:
            print("Name error : ", err)
        
        return 

    if p[0] == 'ASSIGN':
        names[p[1]] = evalExpr(p[2])

    if p[0] == 'PRINT':
        print("CALC >> ", evalExpr(p[1]))
        file_write(evalExpr(p[1]))
    
    if p[0] == 'PRINTSTR':
        print("STR >> ", evalString(p[1]))
        file_write(evalString(p[1]))

    if p[0] == 'IF':
        if evalExpr(p[1]):
            evalInst(p[2])
        else:
            try:
                evalInst(p[3])
            except:
                pass

    if p[0] == 'WHILE':
        while evalExpr(p[1]):
            evalInst(p[2])

    if p[0] == 'SHARP':
        pass

    if p[0] == 'FOR':
        evalInst(p[1])
        while evalExpr(p[2]):
            evalInst(p[4])
            evalInst(p[3])

    if p[0] == "FUNC":
        if p[1][1] != 'empty':
            functions[p[1][0]] = (assignParam(p[1][0],p[1][1], []) ,p[1][2])
        else:
            functions[p[1][0]] = (p[1][1] ,p[1][2])

    if p[0] == "CALL":
        if p[1] in functions:
            evalInst(functions[p[1]][1])
        else:
            raise NameError(f"{p[1]}() function does not exist")
    
    if p[0] == 'CALL_PARAM':
        global isInFunction
        isInFunction = p[1]
        # if(nbArgument != compterTuppleCascadeElement(p[2])):
        #     raise NameError(f'The number of arguments in the function {p[1]}() should be {nbArgument} and not {compterTuppleCascadeElement(p[2])}')
        assignValueParam(p[1],p[2])
        evalInst(functions[p[1]][1])
        isInFunction = False

    return 'UNK'

def evalExpr(p):
    
    if type(p) == int : return p
    if type(p) == str : 
        #--------------------------------
        # Check if it has to serch into local variable or into global
        if isInFunction :
            for x in functions[isInFunction][0]:
                if x[0] == p:
                    return x[1]
        #--------------------------------
        # Global Variable
        elif not(p in names) : raise NameError(f'Variable {p} does not exist')
        return names[p]
        #-------------------------
    if type(p) == tuple:
        if p[0]=='+':return evalExpr(p[1])+evalExpr(p[2])
        if p[0]=='-':return evalExpr(p[1])-evalExpr(p[2])
        if p[0]=='*':return evalExpr(p[1])*evalExpr(p[2])
        if p[0]=='/':return evalExpr(p[1])/evalExpr(p[2])
        
        if p[0]=='==':return evalExpr(p[1]) == evalExpr(p[2])
        if p[0]=='&':return evalExpr(p[1]) and evalExpr(p[2])
        if p[0]=='|':return evalExpr(p[1]) or evalExpr(p[2])
        if p[0]=='<':return evalExpr(p[1]) < evalExpr(p[2])
        if p[0]=='>':return evalExpr(p[1]) > evalExpr(p[2])
        if p[0]=='<=':return evalExpr(p[1]) <= evalExpr(p[2])
        if p[0]=='>=':return evalExpr(p[1]) >= evalExpr(p[2])

    return 'UNK'
    
def compterTuppleCascadeElement(tupple): 
    if(type(tupple) != tuple):
        if(tupple):
            return 1
        else :
            return 0
    else :
        if(tupple[1]) :
            return len(tupple) - 1 + compterTuppleCascadeElement(tupple[1])
        else : return 1

def evalString(p):
    string = ""

    if type(p) is str: string += ' ' + p
    else:
        for i in p:
            if type(i) is str : string += ' ' + i
            elif type(i) is tuple:
                string += evalString(i)

    return string

def assignParam(name, val, rep=[]):
    for param in val:
        if type(param) == tuple:
            if param[0] == 'PARAM':
                assignParam(name, param, rep)
            else:
                rep.append([param[0], param[1]])
        elif type(param) == str and param != 'PARAM':
            rep.append([param, 'undefined'])
    return rep

def getValueParam(name, val, rep=[]):
    if type(val) != tuple:
        val = [val]

    for param in val:
        if type(param) is tuple:
            getValueParam(name, param, rep)
        else :
            rep.append(evalExpr(param))
    return rep

def assignValueParam(name, val):
    listVal = getValueParam(name,val, [])
    for i in range(len(listVal)):
        try :
            functions[name][0][i][1] =  listVal[i] #assignation des valeurs
        except IndexError :
            raise NameError(f"Too many argument : The argument {listVal[i]} does not exist ")
    #----------------------------
    #partie verification que toutes les valeurs ont étés assignés.
    if(isInFunction and functions[isInFunction]):
        nbArgument = len(functions[isInFunction][0]) - 1
        while(nbArgument >= 0 and functions[isInFunction][0][nbArgument][1] != 'undefined') :
            nbArgument -= 1
        if(nbArgument >= 0): 
            raise NameError(f'The number of arguments in the function {isInFunction}() should have {len(functions[isInFunction][0])} defined argument')


def p_start(p):
    '''start : bloc '''
    file_vide()
    p[0] = ('start',p[1])
    printTreeGraph(p[0])
    evalInst(p[1])


def p_bloc(p):
    '''bloc : bloc statement SEMI
            | statement SEMI'''
    if len(p)==3 : p[0] = ('bloc', p[1], 'empty')
    else : p[0] = ('bloc', p[1], p[2])
    
  

def p_statement_assign(p):
    'statement : NAME EQUALS expression'
    p[0] = ('ASSIGN', p[1], p[3])

def p_statement_expr(p):
    'statement : PRINT LPAREN expression RPAREN'
    p[0] = ('PRINT', p[3])

def p_statement_print_str(p):
    'statement : PRINTSTR LPAREN strings RPAREN'
    p[0] = ('PRINTSTR', p[3])


def p_statement_if(p):
    '''statement : IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE
                 | IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE ELSE LACCOLADE bloc RACCOLADE'''
    if len(p)==8 : p[0] = ('IF', p[3], p[6])
    else : p[0] = ('IF', p[3], p[6], p[10])

def p_statement_while(p):
    'statement : WHILE LPAREN expression RPAREN LACCOLADE bloc RACCOLADE'
    p[0] = ('WHILE', p[3], p[6])

def p_statement_for(p):
    'statement : FOR LPAREN NAME EQUALS expression SEMI expression SEMI NAME EQUALS expression RPAREN LACCOLADE bloc RACCOLADE'
    p[0] = ('FOR', ('ASSIGN', p[3],p[5]), p[7], ('ASSIGN', p[9],p[11]), p[14])

def p_statement_funct(p):
    '''statement : FUNC NAME LPAREN RPAREN LACCOLADE bloc RACCOLADE
                 | FUNC NAME LPAREN param RPAREN LACCOLADE bloc RACCOLADE'''
    if len(p)==8 : p[0] = ('FUNC',(p[2], 'empty', p[6]))
    elif len(p)>8 : p[0] = ('FUNC',(p[2], p[4], p[7]))


def p_statement_call_function(p):
    '''statement : NAME LPAREN RPAREN
                 | NAME LPAREN expression RPAREN'''
    if len(p)==4 : p[0] = ('CALL', p[1])
    else : p[0] = ('CALL_PARAM', p[1], p[3])
    
def p_statement_sharp(p):
    'statement : SHARP strings'
    p[0] = ('SHARP', p[2])

def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression INF expression 
                  | expression SUP expression
                  | expression INFEQUAL expression 
                  | expression SUPEQUAL expression 
                  | expression AND expression 
                  | expression OR expression
                  | expression BOOLEQUAL expression '''
    p[0] = (p[2], p[1] , p[3])


def p_expression_uminus(p):
    'expression : MINUS expression'
    p[0] = -p[2]

def p_expression_suite(p):
    '''expression : expression COMA expression'''
    p[0] = (p[1], p[3])

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_expression_name(p):
    'expression : NAME'
    p[0] = p[1]

def p_param_funct(p):
    '''param : NAME COMA param
             | NAME EQUALS expression COMA param
             | NAME EQUALS expression
             | NAME'''
    if len(p)==4 :
        if p[2] == ',':
            p[0] = ("PARAM", p[1], p[3]) 
        else:
            p[0] = ("PARAM", (p[1], p[3]))
    elif len(p)==6 : p[0] = ("PARAM", (p[1], p[3]), p[5]) 
    elif len(p)==2 : p[0] = ("PARAM", p[1])

def p_print_name(p):
    '''strings : NAME strings
               | NAME'''
    if len(p)==3 : p[0] = (p[1], p[2])
    else : p[0] = (p[1])

def p_error(p):
    print("Syntax error at '%s'" % p.value)

import ply.yacc as yacc
yacc.yacc()

s=open("fileCode.txt", "r").read()

yacc.parse(s)

# pprint(functions)