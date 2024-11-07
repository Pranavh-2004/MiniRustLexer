import ply.lex as lex
import ply.yacc as yacc

# Token list
tokens = (
    'WHILE', 'IF', 'ELSE', 'LET', 'FOR', 'PRINT', 'ID', 'NUMBER', 'SEMICOLON', 
    'EQUALS', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN', 'GT', 'LT', 'CHAR', 'FLOAT', 'STRING'
)

# Reserved keywords
reserved = {
    'while': 'WHILE',
    'if': 'IF',
    'else': 'ELSE',
    'let': 'LET',
    'for': 'FOR',
    'print': 'PRINT'
}

# Regular expression rules for simple tokens
t_SEMICOLON = r';'
t_EQUALS = r'='
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_GT = r'>'
t_LT = r'<'
t_ignore = ' \t'

# Token rules for reserved keywords and identifiers
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
    return t

# Token for floating-point numbers
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

# Token for integer numbers
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Token for single character literals
def t_CHAR(t):
    r"'.'"
    t.value = t.value[1]  # Remove surrounding quotes
    return t

# Token for string literals
def t_STRING(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]  # Remove surrounding quotes
    return t

# Track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Error handling
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lexer.lineno}.")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing rules
def p_program(p):
    '''program : statement
               | program statement'''
    pass

# General statement rule
def p_statement(p):
    '''statement : declaration
                 | while_loop
                 | if_statement
                 | for_loop
                 | print_statement'''
    pass

# Variable declaration
def p_declaration(p):
    '''declaration : LET ID EQUALS expression SEMICOLON'''
    print(f"Correct Syntax: Variable declaration: {p[2]} = {p[4]}")

# While loop
def p_while_loop(p):
    '''while_loop : WHILE LPAREN condition RPAREN LBRACE program RBRACE'''
    print(f"Correct Syntax: While loop with condition: {p[3]}")

# If-else statement
def p_if_statement(p):
    '''if_statement : IF LPAREN condition RPAREN LBRACE program RBRACE
                    | IF LPAREN condition RPAREN LBRACE program RBRACE ELSE LBRACE program RBRACE'''
    if len(p) == 8:
        print(f"Correct Syntax: If statement with condition: {p[3]}")
    else:
        print(f"Correct Syntax: If-else statement with condition: {p[3]}")

# For loop
def p_for_loop(p):
    '''for_loop : FOR LPAREN declaration condition SEMICOLON expression RPAREN LBRACE program RBRACE'''
    print(f"Correct Syntax: For loop with initialization: {p[3]}, condition: {p[4]}, increment: {p[6]}")

# Print statement
def p_print_statement(p):
    '''print_statement : PRINT LPAREN expression RPAREN SEMICOLON'''
    print(f"Correct Syntax: Print statement with expression: {p[3]}")

# Condition for if, while, for loops
def p_condition(p):
    '''condition : expression GT expression
                 | expression LT expression
                 | expression EQUALS EQUALS expression'''
    p[0] = f"{p[1]} {p[2]} {p[3]}"

# Expressions (variable, number, or operations)
def p_expression(p):
    '''expression : ID
                  | NUMBER
                  | FLOAT
                  | STRING
                  | CHAR
                  | expression EQUALS expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = f"{p[1]} {p[2]} {p[3]}"

# Error rule for syntax errors
def p_error(p):
    if p is None:
        print("Syntax error: Unexpected end of input.")
    else:
        print(f"Syntax error at '{p.value}' (line {p.lineno})")

# Build the parser
parser = yacc.yacc()

# Test cases
test_cases = [
    "let x = 10;",                           # Variable declaration
    "while (x > 5) { let y = 20; }",         # While loop
    "if (x == 15) { let z = 30; } else { let z = 40; }",  # If-else statement
    "for (let i = 0; i < 10; i = i + 1) { let j = i; }",  # For loop
    "print(x);"                              # Print statement
]

for i in test_cases:
    print(f"\nParsing: {i}")
    lexer.input(i)
    for tok in lexer:
        print(tok)
    parser.parse(i)