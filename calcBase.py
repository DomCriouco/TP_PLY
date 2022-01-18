# -----------------------------------------------------------------------------
# calc.py
#
# Expressions arithmÃ©tiques sans variables
# -----------------------------------------------------------------------------
from genereTreeGraphviz2 import printTreeGraph

reserved = {
    'if' : 'IF',
    'print' : 'PRINT',
    'true' : 'TRUE',
    'false' : 'FALSE',
    'while' : 'WHILE',
    'for' : 'FOR'
}

tokens = (
    'NUMBER','MINUS',
    'PLUS','TIMES','DIVIDE',
    'AND','OR',
    'LESSTHAN','BIGGERTHAN',
    'SEMICOLON', 'EQUAL',
    'LPAREN','RPAREN','LBRACKET','RBRACKET',
    'ISEQUAL','NOTEQUAL',
    'NAME'
    ) + tuple(reserved.values())

# Tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'

t_AND  = r'&'
t_OR  = r'\|'

t_LESSTHAN  = r'<'
t_BIGGERTHAN  = r'>'

t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACKET  = r'\{'
t_RBRACKET  = r'\}'

t_SEMICOLON  = r';'
t_EQUAL  = r'='

t_ISEQUAL  = r'=='
t_NOTEQUAL  = r'!='

functions = {}
vars = {}

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]+'
    t.type = reserved.get(t.value, 'NAME')
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
    ('nonassoc', 'LESSTHAN', 'BIGGERTHAN', 'EQUAL', 'ISEQUAL', 'NOTEQUAL'),
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE'),
)

def p_start(p):
    'start : bloc'
    p[0] = ('START',p[1])
    # print('Arbre de dérivation = ',p[1])
    printTreeGraph(p[0])
    evalInst(p[1])

def p_bloc(p):
    '''bloc : statement SEMICOLON
                | bloc statement SEMICOLON'''
    if len(p) == 4:
        p[0] = ('bloc', p[1], p[2])
    else:
        p[0] = ('bloc', p[1], 'empty')

def p_statement_function(p):
    '''statement : NAME LPAREN statement RPAREN LBRACKET bloc RBRACKET
                | NAME LPAREN RPAREN LBRACKET bloc RBRACKET'''
    if len(p) == 8:
        p[0] = ('function',(p[1],p[3],p[6]))
    else:
        p[0] = ('function',(p[1],'Empty',p[5]))

def p_statement_function_call(p):
    '''statement : NAME LPAREN RPAREN '''
    p[0] = ('call',p[1])

def p_statement_expr(p):
    'statement : PRINT LPAREN expression RPAREN'
    p[0] = ('print',p[3])
    
def p_statement_condition(p):
    'statement : IF LPAREN expression RPAREN LBRACKET bloc RBRACKET'
    p[0] = ('if',p[3],p[6])

def p_statement_for(p):
    'statement : FOR LPAREN statement SEMICOLON expression SEMICOLON statement RPAREN LBRACKET bloc RBRACKET'
    p[0] = ('for',p[3],p[5],p[7],p[10])
    
def p_statement_while(p):
    'statement : WHILE LPAREN expression RPAREN LBRACKET bloc RBRACKET'
    p[0] = ('while',p[3],p[6])

def p_expression_incrementation(p):
    '''statement : NAME PLUS PLUS
                | NAME MINUS MINUS'''
    p[0] = (p[2]+p[3],p[1])

def p_expression_var(p):
    '''expression : NAME'''
    p[0] = p[1]

def p_expression_binop(p):
    '''expression : expression AND expression
                | expression OR expression
                | expression LESSTHAN expression
                | expression BIGGERTHAN expression
                | expression PLUS expression
                | expression MINUS expression
                | expression TIMES expression
				| expression DIVIDE expression
				| expression ISEQUAL expression
				| expression NOTEQUAL expression'''
    p[0] = (p[2],p[1],p[3])

def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]

def p_expression_bool(p):
    '''expression : TRUE
                | FALSE'''
    p[0] = p[1]
    
def p_assignement(p):
    'statement : NAME EQUAL expression'
    p[0] = ('=',p[1], p[3])

def p_error(p):
    print("Syntax error at '%s'" % p.value)

import ply.yacc as yacc
yacc.yacc()

def evalExpr(t):
    # print('evalExpr de ',t)
    if type(t) == int : return t
    if t == 'true': return True
    if t == 'false': return False
    if type(t) == str : return vars[t]
    if type(t) == tuple :
        if t[0] == '&': return evalExpr(t[1]) and evalExpr(t[2])
        if t[0] == '|': return evalExpr(t[1]) or evalExpr(t[2])
        if t[0] == '>': return evalExpr(t[1]) > evalExpr(t[2])
        if t[0] == '<': return evalExpr(t[1]) < evalExpr(t[2])
        if t[0] == '+': return evalExpr(t[1]) + evalExpr(t[2])
        if t[0] == '*': return evalExpr(t[1]) * evalExpr(t[2])
        if t[0] == '-': return evalExpr(t[1]) - evalExpr(t[2])  
        if t[0] == '/': return evalExpr(t[1]) / evalExpr(t[2])
        if t[0] == '==': return evalExpr(t[1]) == evalExpr(t[2])
        if t[0] == '!=': return evalExpr(t[1]) != evalExpr(t[2])
    return 'UNK'
def evalInst(t):
    # print('evalInst de ',t)
    if t[0] == 'function' :
        functions[t[1][0]] = (t[1][1],t[1][2])

    if t == "Empty" : return
    if t[0] == '=' : 
        vars[t[1]] = evalExpr(t[2])
    if t[0] == 'print' : 
        print(evalExpr(t[1]))
    if t[0] == 'bloc' : 
        evalInst(t[1])
        evalInst(t[2])
    if t[0] == 'call' :
        evalInst(functions[t[1]][1])
    if t[0] == 'if' :
        if evalExpr(t[1]) == True:
            evalInst(t[2])
    if t[0] == 'while' :
        while evalExpr(t[1]) == True:
            evalInst(t[2])
    if t[0] == 'for' :
        evalExpr(t[1])
        while evalExpr(t[2]) == True:
            evalInst(t[4])
            evalInst(t[3])
    
    if t[0] == '++' :
        vars[t[1]] = evalExpr(t[1])+1
    if t[0] == '--' :
        vars[t[1]] = evalExpr(t[1])-1
        

s = input('calc > ')
yacc.parse(s)