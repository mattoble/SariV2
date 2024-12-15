import ply.lex as lex
import ply.yacc as yacc
import subprocess


# List of token types used in the Lexer and Parser.
tokens = [
    "IDENTIFIER",           # Variable names and function names.
    "OPERATOR",             # Basic arithmetic operators (+, -, *, /).
    "RELATIONAL_OPERATOR",  # Comparison operators (==, !=, <, >).
    "PAREN_OPEN",           # Opening parenthesis "(".
    "PAREN_CLOSE",          # Closing parenthesis ")".
    "ASSIGN",               # Assignment operator "=".
    "COLON",                # Colon ":" used in various contexts.
    "COMMA",                # Comma "," for separating items.
    "STRING",               # String literals.
    "IF",                   # Conditional statement "if".
    "ELSE",                 # Conditional statement "else".
    "PRINT",                # Print function.
    "INPUT",                # Input function.
    "INTEGER",              # Integer literals.
    "FLOAT"                 # Floating-point literals
]


# Regular expressions for token matching in the Lexer.
t_OPERATOR = r"[+\-*/]"                                     # Matches basic arithmetic operators: +, -, *, /.
t_RELATIONAL_OPERATOR = r"==|≠|>|<|≤|≥"                     # Matches relational operators: ==, ≠, >, <, ≤, ≥.
t_PAREN_OPEN = r"\("                                        # Matches opening parenthesis '('.
t_PAREN_CLOSE = r"\)"                                       # Matches closing parenthesis ')'.
t_ASSIGN = r"="                                             # Matches assignment operator '='.
t_COLON = r":"                                              # Matches colon ':'.
t_COMMA = r","                                              # Matches comma ','.
t_STRING = r'"(?:[^"\\]|\\.|\\n)*"|“(?:[^”\\]|\\.|\\n)*”'   # Matches string literals (supports escaped characters).
t_INTEGER = r"\d+"                                          # Matches integers (whole numbers).
t_FLOAT = r"\d+\.\d+"                                       # Matches floating-point numbers.


# Token function for identifying keywords and identifiers.
def t_IDENTIFIER(t):
    r"[a-zA-Z_][a-zA-Z0-9_]*"                               # Matches variable names and function names.
    keywords = {
        "kung": "IF",                                       # Keyword for "if".
        "kung hindi": "ELSE",                               # Keyword for "else".
        "basahin": "INPUT",                                 # Keyword for "input".
        "ilabas": "PRINT",                                  # Keyword for "print"
    }
    t.type = keywords.get(t.value, "IDENTIFIER")            # Set token type based on keywords.
    return t



# Ignore whitespace characters (spaces, tabs, newlines).
t_ignore = " \t\n"



# Error handling function for illegal characters.
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")              # Print the illegal character.
    t.lexer.skip(1)                                         # Skip the illegal character and continue



# Build the lexer using the defined token rules and functions.
lexer = lex.lex()                                           # Initializes the lexer with the previously defined token specifications.


# Grammar rules and actions for the parser.
def p_program(p):
    '''program : statement
               | program statement'''
    # Constructs a PROGRAM node with either a single statement or multiple statements.
    if len(p) == 2:
        p[0] = Node("PROGRAM", None, [p[1]])
    else:
        p[0] = Node("PROGRAM", None, p[1].children + [p[2]])

def p_statement(p):
    '''statement : print_statement
                  | if_statement
                  | assignment_statement
                  | input_statement'''
    # Sets the current node to the parsed statement.
    p[0] = p[1]

def p_print_statement(p):
    '''print_statement : PRINT PAREN_OPEN expression_list PAREN_CLOSE'''
    # Constructs a PRINT node with the expression list.
    p[0] = Node("PRINT", None, p[3])

def p_expression_list(p):
    '''expression_list : expression
                      | expression_list COMMA expression'''
    # Constructs a list of expressions, handling both single and multiple expressions.
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_expression(p):
    '''expression : INTEGER
                  | FLOAT
                  | IDENTIFIER
                  | STRING
                  | PAREN_OPEN expression PAREN_CLOSE
                  | expression OPERATOR expression
                  | expression RELATIONAL_OPERATOR expression'''
    # Constructs expression nodes based on the type of expression.
    if len(p) == 2:
        if p.slice[1].type == "INTEGER":
            p[0] = Node("INTEGER", int(p[1]))                   # Create INTEGER node.
        elif p.slice[1].type == "FLOAT":
            p[0] = Node("FLOAT", float(p[1]))                   # Create FLOAT node.
        elif p.slice[1].type == "IDENTIFIER":
            p[0] = Node("IDENTIFIER", p[1])                     # Create IDENTIFIER node.
        elif p.slice[1].type == "STRING":
            p[0] = Node("STRING", p[1])                         # Create STRING node.
    elif len(p) == 4:
        if p[1] == "(":
            p[0] = p[2]                                         # Handle parentheses.
        else:
            p[0] = Node("OPERATOR", p[2], [p[1], p[3]])         # Create OPERATOR node.
    elif len(p) == 3:
        p[0] = Node("RELATIONAL_OPERATOR", p[2], [p[1], p[3]])  # Create RELATIONAL_OPERATOR node

def p_if_statement(p):
    '''if_statement : IF expression COLON block ELSE COLON block'''
    # Constructs an IF node with the condition and associated blocks.
    p[0] = Node("IF", None, [p[2], p[4], p[7]])

def p_block(p):
    '''block : statement
             | block statement'''
    # Constructs a BLOCK node with one or more statements.
    if len(p) == 2:
        p[0] = Node("BLOCK", None, [p[1]])
    else:
        p[0] = Node("BLOCK", None, p[1].children + [p[2]])

def p_assignment_statement(p):
    '''assignment_statement : IDENTIFIER ASSIGN expression'''
    # Constructs an ASSIGN node with the identifier and expression.
    p[0] = Node("ASSIGN", p[1], [p[3]])

def p_input_statement(p):
    '''input_statement : INPUT PAREN_OPEN expression PAREN_CLOSE'''
    # Constructs an INPUT node with the expression.
    p[0] = Node("INPUT", None, [p[3]])


# Error rule for handling syntax errors.
def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error at EOF")


# Build the parser.
parser = yacc.yacc()                        # Initializes the parser with the grammar rules.


# Node class for the Abstract Syntax Tree (AST).
class Node:
    def __init__(self, type, value, children=None):
        self.type = type                    # Type of the node (e.g., INTEGER, IDENTIFIER).
        self.value = value                  # Value of the node (e.g., actual number or identifier name).
        self.children = children or []      # List of child nodes.

# Generate C code from the AST
def generate_c_code(ast):
    def generate_expression(node):
        # Recursively generates C code for different node types.
        if node.type == "INTEGER":
            return str(node.value)          # Returns integer as string.
        elif node.type == "FLOAT":
            return str(node.value)          # Returns float as string.
        elif node.type == "IDENTIFIER":
            return node.value               # Returns identifier name.
        elif node.type == "STRING":
            return f'"{node.value[1:-1]}"'  # Returns string literal.
        elif node.type == 'OPERATOR':
            return f"({generate_expression(node.children[0])} {node.value} {generate_expression(node.children[1])})"        # Generates operator expression.
        elif node.type == "RELATIONAL_OPERATOR":
            return f"({generate_expression(node.children[0])} {node.value} {generate_expression(node.children[1])})"        # Generates relational expression.
        raise ValueError(f"Unknown node type: {node.type}")     # Error for unknown node types.                                                    

# Generate C code for various statement types from the AST.
    def generate_statement(node, declared_vars, var_types):
        if node.type == "ASSIGN":
            if node.value not in declared_vars:
                declared_vars.add(node.value)
                if node.children[0].type == "STRING":
                    var_types[node.value] = "char*"
                    return f"char* {node.value} = {generate_expression(node.children[0])};"     # String assignment.
                elif node.children[0].type == "INTEGER":
                    var_types[node.value] = "int"
                    return f"int {node.value} = {generate_expression(node.children[0])};"       # Integer assignment.
                elif node.children[0].type == "FLOAT":
                    var_types[node.value] = "double"
                    return f"double {node.value} = {generate_expression(node.children[0])};"    # Float assignment.
            else:
                # Handles variable reassignments based on type.
                if var_types[node.value] == "char*":
                    return f"{node.value} = {generate_expression(node.children[0])};"
                elif var_types[node.value] == "int":
                    return f"{node.value} = {generate_expression(node.children[0])};"
                elif var_types[node.value] == "double":
                    return f"{node.value} = {generate_expression(node.children[0])};"
        # Handles print statements.
        elif node.type == "PRINT":
            args = []
            for arg in node.children:
                if arg.type == "STRING":
                    args.append(f'"{arg.value[1:-1]}"')     # Handle string literals.
                elif arg.type == "INTEGER":
                    args.append("%d")                       # Format specifier for integers.
                elif arg.type == "FLOAT":
                    args.append("%f")                       # Format specifier for floats.
                else:
                    args.append(generate_expression(arg))   # Handle other expressions.
            return f"printf({", ".join(args)});"            # Construct printf statement.
        # Handles input statements.
        elif node.type == "INPUT":
            if node.children[0].value not in declared_vars:
                declared_vars.add(node.children[0].value)
                var_types[node.children[0].value] = "double"
                return f"double {node.children[0].value};\n{node.children[0].value} = atof(gets(NULL));"    # Declare and assign.
            else:
                return f"{node.children[0].value} = atof(gets(NULL));"      # Reassign.
        # Handles if statements.
        elif node.type == "IF":
            condition = generate_expression(node.children[0])           # Generate condition.
            body = "\n".join(generate_statement(stmt, declared_vars, var_types) for stmt in node.children[1].children)          # Generate body.
            else_part = "\n".join(generate_statement(stmt, declared_vars, var_types) for stmt in node.children[2].children) if node.children[2] else ""     # Generate else body.
            return f"if ({condition}) {{\n{body}\n}} else {{\n{else_part}\n}}"          # Construct if-else statement.
         # Handles block statements.
        elif node.type == "BLOCK":
            return "\n".join(generate_statement(stmt, declared_vars, var_types) for stmt in node.children)          # Generate block.
        raise ValueError(f"Unknown node type: {node.type}")

# Generate C code for the entire program from the AST.
    def generate_program(node):
        if node.type == "PROGRAM":
            declared_vars = set()       # Track declared variables.
            var_types = {}              # Track variable types.
            statements = "\n".join(generate_statement(stmt, declared_vars, var_types) for stmt in node.children)            # Generate all statements.
            return statements
        raise ValueError(f"Unknown node type: {node.type}")         # Error for unknown node types.


    # Main function to generate complete C code.
    c_code = generate_program(ast)
    return f"#include <stdio.h>\n#include <stdlib.h>\n\nint main() {{\n{c_code}\nreturn 0;\n}}"


def compile_sari(input_file, output_file):
    """
    Compiles Sari code from the input file to C code and compiles it.

    Args:
        input_file (str): The path to the input Sari code file.
        output_file (str): The path where the generated C code will be saved.
    """

    with open(input_file, "r", encoding="utf-8") as f:
        sari_code = f.read()            # Read the Sari code from the input file.

    print("Compiling Sari code")
    ast = parser.parse(sari_code, lexer=lexer)          # Parse the Sari code to create an AST.
    print("AST:", ast)                                  # Output the generated AST.
    c_code = generate_c_code(ast)                       # Generate C code from the AST.

    with open(output_file, "w", encoding="utf-8") as f:
        f.write(c_code)                                 # Write the generated C code to the output file.

    # Compile the generated C code into an executable.
    subprocess.run(["gcc", "-o", "sari_program", output_file], check=True)

if __name__ == "__main__":
    compile_sari("main.sari", "main.c")                 # Compile the Sari code in 'main.sari' to 'main.c'
