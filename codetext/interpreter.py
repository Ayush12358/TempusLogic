import re

# --- 1. TOKENIZER (LEXER) ---
# Turns the code string into a stream of tokens

class Token:
    def __init__(self, type, value):
        self.type = type
        self.value = value
    def __repr__(self):
        return f"Token({self.type}, {repr(self.value)})"

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos] if self.pos < len(text) else None
        
        # Regex for all token types
        self.token_specs = [
            ('COMMENT',   r'@[^\n]*'),       # @ comments
            ('NUMBER',    r'\d+'),           # Numbers
            ('ID',        r'[a-zA-Z_][a-zA-Z0-9_]*'), # IDs (vars, keywords)
            ('ASSIGN',    r'->'),           # Assignment
            ('GTE_INV',   r'=>'),           # Inverted GTE
            ('EQUALS',    r'=='),           # Equals
            ('GT',        r'>'),            # Greater than
            ('LPAREN',    r'\('),           # (
            ('RPAREN',    r'\)'),           # )
            ('LBRACKET',  r'\['),           # [
            ('RBRACKET',  r'\]'),           # ]
            ('LBRACE',    r'\{'),           # {
            ('RBRACE',    r'\}'),           # }
            ('PLUS',      r'\+'),           # +
            ('MINUS',     r'-'),           # -
            ('STAR',      r'\*'),           # *
            ('COLON',     r':'),            # :
            ('NEWLINE',   r'\n'),           # Newlines (for ignoring)
            ('SKIP',      r'[ \t]+'),      # Whitespace (for ignoring)
            ('MISMATCH',  r'.'),            # Any other char (error)
        ]
        # Compile regex
        self.token_regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in self.token_specs)
        self.token_iter = re.finditer(self.token_regex, self.text)
        
        # Keyword mapping
        self.keywords = {
            'main': Token('MAIN', 'main'),
            'bool': Token('BOOL', 'bool'),
            'int': Token('INT', 'int'),
            'if': Token('IF', 'if'),
            'though_if': Token('THOUGH_IF', 'though_if'),
            'doth': Token('DOTH', 'doth'),
            'while': Token('WHILE', 'while'),
            'return': Token('RETURN', 'return'),
            'truth': Token('TRUTH', 'truth'),
            'false': Token('FALSE', 'false'),
        }

    def get_next_token(self):
        try:
            match = next(self.token_iter)
            kind = match.lastgroup
            value = match.group()

            if kind == 'NEWLINE' or kind == 'SKIP' or kind == 'COMMENT':
                return self.get_next_token() # Ignore and get next
            
            if kind == 'ID':
                # Check if it's a keyword
                return self.keywords.get(value, Token('ID', value))
            
            if kind == 'MISMATCH':
                raise RuntimeError(f'Lexer error: Unexpected character {value}')
                
            return Token(kind, value)
            
        except StopIteration:
            return Token('EOF', None) # End of File

# --- 2. ABSTRACT SYNTAX TREE (AST) NODES ---
# Defines the structure of the program

class ASTNode: pass
class Program(ASTNode):
    def __init__(self, body):
        self.body = body # body is a Block node
class Block(ASTNode):
    def __init__(self, statements):
        self.statements = statements
class Assignment(ASTNode):
    def __init__(self, expression, var_name):
        self.expression = expression
        self.var_name = var_name # String
class IfStatement(ASTNode):
    def __init__(self, if_branch, though_if_branches):
        self.if_branch = if_branch # (condition, block) tuple
        self.though_if_branches = though_if_branches # list of (cond, block)
class DothWhile(ASTNode):
    def __init__(self, body, condition):
        self.body = body
        self.condition = condition
class Return(ASTNode):
    def __init__(self, value):
        self.value = value
class Condition(ASTNode):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op # Token
        self.right = right
class BinOp(ASTNode):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op # Token
        self.right = right
class Variable(ASTNode):
    def __init__(self, name):
        self.name = name # String
class Number(ASTNode):
    def __init__(self, value):
        self.value = int(value)
class Boolean(ASTNode):
    def __init__(self, value):
        self.value = value # Python True/False

# --- 3. PARSER ---
# Turns the token stream into an AST

class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            raise RuntimeError(f'Parser error: Expected {token_type}, got {self.current_token.type}')

    def parse(self):
        """Program: main (bool|int) [ statements ]"""
        self.eat('MAIN')
        self.eat('LPAREN')
        
        if self.current_token.type == 'BOOL':
            self.eat('BOOL')
        elif self.current_token.type == 'INT':
            self.eat('INT')
        else:
            raise RuntimeError("Parser error: Expected 'bool' or 'int' return type")
            
        self.eat('RPAREN')
        
        self.eat('LBRACKET')
        statements = self.parse_statements()
        self.eat('RBRACKET')
        
        self.eat('EOF')
        return Program(Block(statements))

    def parse_statements(self):
        """A list of statements, ending at a RBRACKET"""
        statements = []
        while self.current_token.type != 'RBRACKET':
            statements.append(self.parse_statement())
        return statements

    def parse_statement(self):
        """Statement: Assignment, If, Loop, Return, or nested Block"""
        token_type = self.current_token.type
        
        if token_type in ('LPAREN', 'NUMBER', 'ID', 'TRUTH', 'FALSE'):
            # This must be an assignment
            return self.parse_assignment()
        elif token_type == 'IF':
            return self.parse_if_statement()
        elif token_type == 'DOTH':
            return self.parse_doth_while()
        elif token_type == 'RETURN':
            return self.parse_return()
        elif token_type == 'LBRACKET':
            # Nested block
            self.eat('LBRACKET')
            statements = self.parse_statements()
            self.eat('RBRACKET')
            return Block(statements)
        else:
            raise RuntimeError(f'Parser error: Unexpected token {self.current_token}')

    def parse_assignment(self):
        """Assignment: expression -> ID :"""
        expr = self.parse_expression()
        self.eat('ASSIGN')
        var_name = self.current_token.value
        self.eat('ID')
        self.eat('COLON')
        return Assignment(expr, var_name)
    
    def parse_return(self):
        """Return: return (truth | false | expression) :"""
        self.eat('RETURN')
        
        if self.current_token.type == 'TRUTH':
            val = Boolean(True)
            self.eat('TRUTH')
        elif self.current_token.type == 'FALSE':
            val = Boolean(False)
            self.eat('FALSE')
        else:
            # It's an expression (number, var, or operation)
            val = self.parse_expression()
            
        self.eat('COLON')
        return Return(val)

    def parse_if_statement(self):
        """If: if {cond} [block] (though_if {cond} [block])*"""
        # 'if' branch
        self.eat('IF')
        cond = self.parse_condition()
        block = self.parse_block()
        if_branch = (cond, block)
        
        # 'though_if' branches
        though_if_branches = []
        while self.current_token.type == 'THOUGH_IF':
            self.eat('THOUGH_IF')
            cond = self.parse_condition()
            block = self.parse_block()
            though_if_branches.append((cond, block))
            
        return IfStatement(if_branch, though_if_branches)

    def parse_doth_while(self):
        """Loop: doth [block] while {cond}"""
        self.eat('DOTH')
        body = self.parse_block()
        self.eat('WHILE')
        cond = self.parse_condition()
        # The 'doth' statement itself does not end with a colon
        return DothWhile(body, cond)

    def parse_block(self):
        """Block: [ statements ]"""
        self.eat('LBRACKET')
        statements = self.parse_statements()
        self.eat('RBRACKET')
        return Block(statements)

    def parse_condition(self):
        """Condition: { expression op expression }"""
        self.eat('LBRACE')
        left = self.parse_expression()
        op = self.current_token
        if op.type not in ('EQUALS', 'GT', 'GTE_INV'):
            raise RuntimeError(f'Parser error: Invalid condition operator {op.type}')
        self.eat(op.type)
        right = self.parse_expression()
        self.eat('RBRACE')
        return Condition(left, op, right)

    def parse_expression(self):
        """Expression: ( expression op expression ) | NUMBER | ID | TRUTH | FALSE"""
        token = self.current_token
        
        if token.type == 'LPAREN':
            # This is a ( expr op expr )
            self.eat('LPAREN')
            left = self.parse_expression()
            op = self.current_token
            if op.type not in ('PLUS', 'MINUS', 'STAR'):
                raise RuntimeError(f'Parser error: Invalid binary operator {op.type}')
            self.eat(op.type)
            right = self.parse_expression()
            self.eat('RPAREN')
            return BinOp(left, op, right)
            
        elif token.type == 'NUMBER':
            self.eat('NUMBER')
            return Number(token.value)
            
        elif token.type == 'ID':
            self.eat('ID')
            return Variable(token.value)
            
        elif token.type == 'TRUTH':
            self.eat('TRUTH')
            return Boolean(True)
            
        elif token.type == 'FALSE':
            self.eat('FALSE')
            return Boolean(False)
            
        else:
            raise RuntimeError(f'Parser error: Unexpected token in expression {token}')

# --- 4. INTERPRETER ---
# Walks the AST and executes the program

class ReturnValue(Exception):
    """Custom exception for handling 'return' statements"""
    def __init__(self, value):
        self.value = value

class Interpreter:
    def __init__(self, parser):
        self.parser = parser
        self.environment = {} # Variable storage

    def visit(self, node):
        """Dispatches to the correct visit method"""
        method_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise RuntimeError(f'No visit_{type(node).__name__} method')

    def visit_Program(self, node):
        return self.visit(node.body)

    def visit_Block(self, node):
        for stmt in node.statements:
            self.visit(stmt)

    def visit_Assignment(self, node):
        value = self.visit(node.expression)
        self.environment[node.var_name] = value

    def visit_IfStatement(self, node):
        # Visit 'if'
        if_cond, if_block = node.if_branch
        if self.visit(if_cond):
            self.visit(if_block)
            return # Only execute one branch
        
        # Visit 'though_if' branches
        for cond, block in node.though_if_branches:
            if self.visit(cond):
                self.visit(block)
                return # Only execute one branch

    def visit_DothWhile(self, node):
        while self.visit(node.condition):
            self.visit(node.body)

    def visit_Return(self, node):
        raise ReturnValue(self.visit(node.value))

    def visit_Condition(self, node):
        left_val = self.visit(node.left)
        right_val = self.visit(node.right)
        op = node.op.type
        
        # Apply the obfuscated logic
        if op == 'EQUALS':
            return left_val == right_val
        if op == 'GT':
            return left_val > right_val
        if op == 'GTE_INV':
            # This is the 'X => Y' -> 'Y >= X' rule
            return right_val >= left_val
            
        raise RuntimeError(f'Interpreter error: Unknown condition op {op}')

    def visit_BinOp(self, node):
        left_val = self.visit(node.left)
        right_val = self.visit(node.right)
        op = node.op.type
        
        # Apply the INSANE '1 - b' -> 'b - 1' rule
        if op == 'PLUS':
            return right_val + left_val
        if op == 'MINUS':
            return right_val - left_val
        if op == 'STAR':
            return right_val * left_val
            
        raise RuntimeError(f'Interpreter error: Unknown binary op {op}')

    def visit_Variable(self, node):
        var_name = node.name
        if var_name not in self.environment:
            raise RuntimeError(f'Interpreter error: Variable {var_name} not defined')
        return self.environment[var_name]

    def visit_Number(self, node):
        return node.value

    def visit_Boolean(self, node):
        return node.value

    def interpret(self):
        try:
            ast = self.parser.parse()
            self.visit(ast)
        except ReturnValue as ret:
            # This is the "success" case
            return ret.value
        
        # If the program finishes without a return, that's a problem
        raise RuntimeError("Interpreter error: Program finished without a 'return' statement.")


# --- 5. HELPER FUNCTION ---

def get_ground_truth(code_string: str) -> str:
    """
    Interprets a smol code string and returns the ground truth output.
    Returns the output as a string ('truth', 'false', or a number str).
    Returns 'INTERPRETER_ERROR: ...' on failure.
    """
    try:
        lexer = Lexer(code_string)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        
        output = interpreter.interpret()
        
        # Convert Python bool to 'truth'/'false'
        if isinstance(output, bool):
            return "truth" if output else "false"
        else:
            return str(output)
            
    except Exception as e:
        return f"INTERPRETER_ERROR: {e}"