from genereTreeGraphviz2 import printTreeGraph
# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables.
# -----------------------------------------------------------------------------
reserved = {
    'print' : 'PRINT',
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'for'   : 'FOR',
    'function': 'FUNC'
}

tokens = [
    'NAME','NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','EQUALS',
    'LPAREN','RPAREN','LACCOLADE', 'RACCOLADE', 'SEMI',
    'INF', 'SUP', 'AND', 'OR', 'BOOLEQUAL',
#    'PEQUAL', 'MEQUAL'
    ] + list(reserved.values())
# Tokens


t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LACCOLADE = r'\{'
t_RACCOLADE = r'\}'
t_SEMI    = r';'
t_INF     = r'<'
t_SUP     = r'>'
t_AND     = r'&'
t_OR     = r'\|'
t_BOOLEQUAL = r'=='
#t_PEQUAL = r'\+='
#t_MEQUAL = r'-='


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

# Build the lexer
import ply.lex as lex
lex.lex()

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'INF', 'SUP', 'BOOLEQUAL'),
    ('left','PLUS', 'MINUS'),
    ('left','TIMES','DIVIDE'),
    #('right','UMINUS')
)
# dictionary of names (for storing variables)

def evalInst(p):
    print("evaInst de ",p)
    if p == 'empty' : return
    if type(p)!=tuple : 
        print("inst non tuple")
        return
    
    if p[0]=='bloc':
        evalInst(p[1])
        evalInst(p[2])
        return
        
    if p[0]=='ASSIGN':
        names[p[1]]=evalExpr(p[2])
#    if p[0]=='ASSIGNPLUS':
#        name[p[1]] = name[p[1]]+evalExpr(p[2])
#    if p[0]=='ASSIGNLESS':
#        name[p[1]] = name[p[1]]-evalExpr(p[2])
    
    if p[0]=='PRINT':
        print("CALC> ", evalExpr(p[1]))
    
    if(p[0] == 'IF'):
        if(evalExpr(p[1])):
            evalInst(p[2])
        elif(len(p) == 4):
            evalInst(p[3])

    if(p[0] == 'WHILE'):
        while(evalExpr(p[1])):
            evalInst(p[2])

    if(p[0] == 'FOR'):
        evalInst(p[1])
        while(evalExpr[2]):
            evalInst(p[4])
            evalInst(p[3])
        
    return 'UNK'
    
names={}
functions={} #{NOM : (PARAMETRE, CORPS)}

def evalExpr(p):
    if type(p) == int : return p
    if type(p) == str : return names[p]
    if type(p)==tuple : 
        if p[0]=='+':return evalExpr(p[1])+evalExpr(p[2])
        if p[0]=='-':return evalExpr(p[1])-evalExpr(p[2])
        if p[0]=='*':return evalExpr(p[1])*evalExpr(p[2])
        if p[0]=='/':return evalExpr(p[1])/evalExpr(p[2])
        
        if p[0]=='==':return evalExpr(p[1]) == evalExpr(p[2])
        if p[0]=='&':return evalExpr(p[1]) and evalExpr(p[2])
        if p[0]=='\|':return evalExpr(p[1]) or evalExpr(p[2])
        if p[0]=='<':return evalExpr(p[1]) < evalExpr(p[2])
        if p[0]=='>':return evalExpr(p[1]) > evalExpr(p[2])
    return 'UNK'

def p_start(p):
    ''' start : bloc '''
    print(p[1])
    printTreeGraph(p[1])
    evalInst(p[1])
    
    
def p_bloc(p):
    '''bloc : bloc statement SEMI
            | statement SEMI'''
    if len(p)==3 : p[0]=('bloc', p[1],'empty')
    else : p[0]=('bloc', p[1],p[2])

def p_statement_assign(p):
    '''statement : NAME EQUALS expression''' #| NAME PEQUAL expression
                                             #| NAME MEQUAL expression 
    if(p[3] == '='):
        p[0]=('ASSIGN', p[1],p[3])
#    if(p[3] == '+='):
#        p[0]=('ASSIGNPLUS',p[1],p[3])
#    if(p[3] == '-='):
#        p[0]=('ASSIGNLESS',p[1],p[3])
    

def p_statement_expr(p):
    '''statement : PRINT LPAREN expression RPAREN
                 | IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE ELSE LACCOLADE bloc RACCOLADE
                 | IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE
                 | WHILE LPAREN expression RPAREN LACCOLADE bloc RACCOLADE
                 | FOR LPAREN NAME EQUALS expression SEMI expression SEMI bloc RPAREN LACCOLADE bloc RACCOLADE'''
    #print("entrée dans expr\n") #debug
    if(p[1] == 'if'):
        #print("detection if\n") #debug
        if(len(p) >= 8 and p[8] == 'else'):
            #print("ecriture du if else") #debug
            p[0] = ('IF',p[3], p[6],p[10])#(IF, CONDITION, DO, ELSE)
        else:
            #print("ecriture du if sans else")#debug
            p[0] = ('IF',p[3], p[6]) 
    if(p[1] == 'print'):
        p[0] = ('PRINT', p[3])
    if(p[1] == 'while'):
        p[0] = ('WHILE', p[3],p[6]) #(WHILE, CONDITION, DO)
    if(p[1] == 'for'):
        p[0] = (p[1],('ASSIGN', p[3],p[5]),p[7],p[9],p[12])#(FOR,ASSIGNATION,CONDITION,RETOURLIGNE,DO)


#def p_statement_while(p):
#    'statement : WHILE LPAREN expression RPAREN LACCOLADE bloc RACCOLADE'
#    p[0] = ('WHILE', p[3],p[6]) #(WHILE, CONDITION, DO)

#def p_statement_for(p):
#    'statement : FOR LPAREN statement SEMI expression SEMI expression RPAREN LACCOLADE bloc RACCOLADE'
#    p[0] = ('FOR',p[3],p[5],p[7],p[10])

#def p_statement_for(p):
#    'statement : FOR LPAREN NAME EQUALS expression SEMI expression SEMI expression RPAREN LACCOLADE bloc RACCOLADE'
#    p[0] = (p[1],('ASSIGN', p[3],p[5]),p[7],p[9],p[12])
    

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
    p[0] = (p[2],p[1] , p[3])


def p_expression_uminus(p):
    'expression : MINUS expression '
    p[0] = -p[2]

def p_expression_group(p):
    '''expression : LPAREN expression RPAREN
                  | LACCOLADE expression RACCOLADE'''
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_expression_name(p):
    'expression : NAME'
    p[0] = p[1]

def p_error(p):
    print("Syntax error at '%s'" % p.value)

import ply.yacc as yacc
yacc.yacc()

s='for(x=0;x<2;x=x+1;){print(x);};'
#s='print(1<2 & 2<1);x=17;print(x+4);'
#s='print(1+2);x=4;x=x+1;'
#s='x=4;x=x+1;print(x+1);'
yacc.parse(s)