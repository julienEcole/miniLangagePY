from genereTreeGraphviz2 import printTreeGraph
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
    'LPAREN','RPAREN', 'SEMI',
    'INF', 'SUP', 'AND', 'OR', 'LACCOLADE', 'RACCOLADE', 'BOOLEQUAL'] + list(reserved.values())
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
t_AND     = r'&'
t_LACCOLADE  = r'{'
t_RACCOLADE  = r'}'
t_SUP     = r'>'
t_OR     = r'\|'
t_BOOLEQUAL = r'=='


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

def file_read():
    f = open("fileCode.txt", "r")
    return f.read()

# Build the lexer
import ply.lex as lex
lex.lex()

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'INF', 'SUP', 'BOOLEQUAL'),
    ('left','PLUS', 'MINUS'),
    ('left','TIMES','DIVIDE'),
)
 
# dictionary of names (for storing variables)
names = {}
functions={} #{NOM : (PARAMETRE, CORPS)}

def evalInst(p):

    if p == 'empty' : return 

    if type(p) != tuple : 
        print("inst non tuple")
        return 

    if p[0] == 'bloc':
        evalInst(p[1])
        evalInst(p[2])
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

    if p[0] == 'WHILE':
        while evalExpr(p[1]):
            evalInst(p[2])

    if p[0] == 'FOR':
        evalInst(p[1])
        while evalExpr(p[2]):
            evalInst(p[4])
            evalInst(p[3])

    if p[0] == "FUNC":
        functions[p[1][0]] = (p[1][1] ,p[1][2])

    if p[0] == "CALL":
        if p[1] in functions:
            evalInst(functions[p[1]][1])
        else:
            print(f"Erreur: La fonction ->{p[1]}() n'existe pas")

    return 'UNK'

def evalExpr(p):
    
    if type(p) == int : return p
    if type(p) == str : return names[p]
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

    return 'UNK'
    
def evalString(p):
    string = ""

    if type(p) is str: string += ' ' + p
    else:
        for i in p:
            if type(i) is str : string += ' ' + i
            elif type(i) is tuple:
                string += evalString(i)

    return string


def p_start(p):
    '''start : bloc '''
    file_vide()
    p[0] = ('start',p[1])
    # print(p[0])
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
    'statement : IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE'
    p[0] = ('IF', p[3], p[6])

def p_statement_while(p):
    'statement : WHILE LPAREN expression RPAREN LACCOLADE bloc RACCOLADE'
    p[0] = ('WHILE', p[3], p[6])

def p_statement_for(p):
    'statement : FOR LPAREN NAME EQUALS expression SEMI expression SEMI NAME EQUALS expression RPAREN LACCOLADE bloc RACCOLADE'
    p[0] = ('FOR', ('ASSIGN', p[3],p[5]), p[7], ('ASSIGN', p[9],p[11]), p[14])

def p_statement_funct(p):
    'statement : FUNC NAME LPAREN RPAREN LACCOLADE bloc RACCOLADE'
    p[0] = ('FUNC',(p[2], 'param', p[6]))

def p_statement_call_function(p):
    'statement : NAME LPAREN RPAREN'
    p[0] = ('CALL', p[1])
    

def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression INF expression 
                  | expression SUP expression 
                  | expression AND expression 
                  | expression OR expression
                  | expression BOOLEQUAL expression '''
    p[0] = (p[2], p[1] , p[3])


def p_expression_uminus(p):
    'expression : MINUS expression'
    p[0] = -p[2]

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_expression_name(p):
    'expression : NAME'
    p[0] = p[1]

def p_print_name(p):
    '''strings : NAME strings
               | NAME'''
    if len(p)==3 : p[0] = (p[1], p[2])
    else : p[0] = (p[1])

def p_error(p):
    print("Syntax error at '%s'" % p.value)

import ply.yacc as yacc
yacc.yacc()


# s = input('INPUT >>> ')
s='print(1+2);x=4;x=x+1;'
s='x=4;x=x+1;if(x<10){print(1+2);};'

s=file_read()



yacc.parse(s)
