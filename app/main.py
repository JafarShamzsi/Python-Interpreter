import sys
from enum import Enum

# Add a custom runtime error class
class LoxRuntimeError(Exception):
    """Runtime error in Lox code."""
    def __init__(self, token, message):
        self.token = token  # The token that caused the error
        self.message = message
        super().__init__(self.message)

class TokenType(Enum):
    # Single-character tokens
    LEFT_PAREN = "LEFT_PAREN"
    RIGHT_PAREN = "RIGHT_PAREN"
    LEFT_BRACE = "LEFT_BRACE"
    RIGHT_BRACE = "RIGHT_BRACE"
    COMMA = "COMMA"
    DOT = "DOT"
    MINUS = "MINUS"
    PLUS = "PLUS"
    SEMICOLON = "SEMICOLON"
    STAR = "STAR"
    SLASH = "SLASH"          # Token type for '/'
    
    # One or two character tokens
    BANG = "BANG"
    BANG_EQUAL = "BANG_EQUAL"
    EQUAL = "EQUAL"
    EQUAL_EQUAL = "EQUAL_EQUAL"
    LESS = "LESS"
    LESS_EQUAL = "LESS_EQUAL"
    GREATER = "GREATER"
    GREATER_EQUAL = "GREATER_EQUAL"
    
    # Literals
    STRING = "STRING"        # Token type for string literals
    NUMBER = "NUMBER"        # Token type for number literals
    IDENTIFIER = "IDENTIFIER"  # New token type for identifiers
    
    # Reserved keywords
    AND = "AND"
    CLASS = "CLASS"
    ELSE = "ELSE"
    FALSE = "FALSE"
    FOR = "FOR"
    FUN = "FUN"
    IF = "IF"
    NIL = "NIL"
    OR = "OR"
    PRINT = "PRINT"
    RETURN = "RETURN"
    SUPER = "SUPER"
    THIS = "THIS"
    TRUE = "TRUE"
    VAR = "VAR"
    WHILE = "WHILE"
    
    # End of file
    EOF = "EOF"

class Token:
    def __init__(self, token_type, lexeme, literal=None, line=1):
        self.token_type = token_type
        self.lexeme = lexeme
        self.literal = literal
        self.line = line  # Add line number
    
    def __str__(self):
        return f"{self.token_type.value} {self.lexeme} {self.literal if self.literal is not None else 'null'}"

# AST classes for expressions
class Expr:
    """Base class for all expressions."""
    pass

class Binary(Expr):
    """Binary expression with two operands and an operator."""
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right
    
    def accept(self, visitor):
        return visitor.visit_binary_expr(self)

class Grouping(Expr):
    """Expression inside parentheses."""
    def __init__(self, expression):
        self.expression = expression
    
    def accept(self, visitor):
        return visitor.visit_grouping_expr(self)

class Literal(Expr):
    """Literal value (number, string, true, false, nil)."""
    def __init__(self, value):
        self.value = value
    
    def accept(self, visitor):
        return visitor.visit_literal_expr(self)

class Unary(Expr):
    """Unary expression with one operand and an operator."""
    def __init__(self, operator, right):
        self.operator = operator
        self.right = right
    
    def accept(self, visitor):
        return visitor.visit_unary_expr(self)

# AST Printer for generating the output format
class AstPrinter:
    """Prints an AST in a lisp-like format."""
    
    def print(self, expr):
        return expr.accept(self)
    
    def visit_binary_expr(self, expr):
        return self.parenthesize(expr.operator.lexeme, expr.left, expr.right)
    
    def visit_grouping_expr(self, expr):
        return self.parenthesize("group", expr.expression)
    
    def visit_literal_expr(self, expr):
        if expr.value is None:
            return "nil"
        if expr.value is True:
            return "true"
        if expr.value is False:
            return "false"
        return str(expr.value)
    
    def visit_unary_expr(self, expr):
        return self.parenthesize(expr.operator.lexeme, expr.right)
    
    def parenthesize(self, name, *exprs):
        builder = []
        builder.append("(")
        builder.append(name)
        
        for expr in exprs:
            builder.append(" ")
            builder.append(expr.accept(self))
        
        builder.append(")")
        
        return "".join(builder)

# Parser for building the AST
class Parser:
    """Parses tokens into an AST."""
    
    def __init__(self, tokens):
        self.tokens = tokens
        self.current = 0
        self.had_error = False
    
    def parse(self):
        """Parse tokens into an expression."""
        try:
            return self.expression()
        except Exception as error:
            self.had_error = True
            print(f"Error: {error}", file=sys.stderr)
            return None
    
    def expression(self):
        """Parse an expression."""
        return self.equality()
    
    def equality(self):
        """Parse an equality comparison (==, !=)."""
        expr = self.comparison()
        
        # Look for equality operators
        while self.match(TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL):
            operator = self.previous()  # Get the operator token
            right = self.comparison()   # Parse the right operand
            expr = Binary(expr, operator, right)  # Create a binary expression
        
        return expr
    
    def comparison(self):
        """Parse a comparison (>, <, >=, <=)."""
        expr = self.term()
        
        # Look for comparison operators
        while self.match(TokenType.GREATER, TokenType.GREATER_EQUAL, 
                         TokenType.LESS, TokenType.LESS_EQUAL):
            operator = self.previous()  # Get the operator token
            right = self.term()  # Parse the right operand
            expr = Binary(expr, operator, right)  # Create a binary expression
        
        return expr
    
    def term(self):
        """Parse a term (addition and subtraction)."""
        expr = self.factor()
        
        # Look for + or - operators
        while self.match(TokenType.PLUS, TokenType.MINUS):
            operator = self.previous()  # Get the operator token
            right = self.factor()  # Parse the right operand
            expr = Binary(expr, operator, right)  # Create a binary expression
        
        return expr
    
    def factor(self):
        """Parse a factor (multiplication and division)."""
        expr = self.unary()
        
        # Look for * or / operators
        while self.match(TokenType.STAR, TokenType.SLASH):
            operator = self.previous()  # Get the operator token
            right = self.unary()  # Parse the right operand
            expr = Binary(expr, operator, right)  # Create a binary expression
        
        return expr
    
    def unary(self):
        """Parse a unary expression."""
        # Check for unary operators (! or -)
        if self.match(TokenType.BANG, TokenType.MINUS):
            operator = self.previous()  # Save the operator token
            right = self.unary()  # Parse the operand (recursively)
            return Unary(operator, right)
        
        # If no unary operator, parse as a primary expression
        return self.primary()
    
    def primary(self):
        """Parse a primary expression."""
        if self.match(TokenType.FALSE):
            return Literal(False)
        if self.match(TokenType.TRUE):
            return Literal(True)
        if self.match(TokenType.NIL):
            return Literal(None)
        
        if self.match(TokenType.NUMBER, TokenType.STRING):
            return Literal(self.previous().literal)
        
        # Handle parenthesized expressions
        if self.match(TokenType.LEFT_PAREN):
            # Parse the expression inside the parentheses
            expr = self.expression()
            
            # Expect a closing parenthesis
            self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
            
            # Return the expression wrapped in a Grouping node
            return Grouping(expr)
        
        raise Exception("Expect expression.")
    
    def match(self, *types):
        """Check if the current token matches any of the given types."""
        for token_type in types:
            if self.check(token_type):
                self.advance()
                return True
        
        return False
    
    def check(self, token_type):
        """Check if the current token is of the given type."""
        if self.is_at_end():
            return False
        return self.peek().token_type == token_type
    
    def advance(self):
        """Advance to the next token."""
        if not self.is_at_end():
            self.current += 1
        return self.previous()
    
    def is_at_end(self):
        """Check if we've reached the end of the tokens."""
        return self.peek().token_type == TokenType.EOF
    
    def peek(self):
        """Get the current token."""
        return self.tokens[self.current]
    
    def previous(self):
        """Get the previous token."""
        return self.tokens[self.current - 1]
    
    def consume(self, token_type, message):
        """Consume the current token if it's of the given type."""
        if self.check(token_type):
            return self.advance()
        
        raise Exception(message)

class Scanner:
    def __init__(self, source):
        self.source = source
        self.tokens = []
        self.start = 0
        self.current = 0
        self.line = 1  # Track line number for error reporting
        self.had_error = False  # Track if any errors occurred
        
        # Reserved keywords mapping
        self.keywords = {
            "and": TokenType.AND,
            "class": TokenType.CLASS,
            "else": TokenType.ELSE,
            "false": TokenType.FALSE,
            "for": TokenType.FOR,
            "fun": TokenType.FUN,
            "if": TokenType.IF,
            "nil": TokenType.NIL,
            "or": TokenType.OR,
            "print": TokenType.PRINT,
            "return": TokenType.RETURN,
            "super": TokenType.SUPER,
            "this": TokenType.THIS,
            "true": TokenType.TRUE,
            "var": TokenType.VAR,
            "while": TokenType.WHILE
        }
    
    def scan_tokens(self):
        """Scan all tokens in the source."""
        while not self.is_at_end():
            self.start = self.current
            self.scan_token()
        
        # Add EOF token at the end
        self.tokens.append(Token(TokenType.EOF, "", None))
        return self.tokens
    
    def scan_token(self):
        """Scan a single token."""
        c = self.advance()
        
        if c == '(':
            self.add_token(TokenType.LEFT_PAREN)
        elif c == ')':
            self.add_token(TokenType.RIGHT_PAREN)
        elif c == '{':
            self.add_token(TokenType.LEFT_BRACE)
        elif c == '}':
            self.add_token(TokenType.RIGHT_BRACE)
        elif c == ',':
            self.add_token(TokenType.COMMA)
        elif c == '.':
            self.add_token(TokenType.DOT)
        elif c == '-':
            self.add_token(TokenType.MINUS)
        elif c == '+':
            self.add_token(TokenType.PLUS)
        elif c == ';':
            self.add_token(TokenType.SEMICOLON)
        elif c == '*':
            self.add_token(TokenType.STAR)
        elif c == '/':
            # Check if it's a comment (//) or division operator (/)
            if self.match('/'):
                # Comment - consume characters until end of line
                while not self.is_at_end() and self.peek() != '\n':
                    self.advance()
            else:
                # Division operator
                self.add_token(TokenType.SLASH)
        elif c == '!':
            # Check if it's '!=' (inequality) or just '!' (negation)
            if self.match('='):
                self.add_token(TokenType.BANG_EQUAL)
            else:
                self.add_token(TokenType.BANG)
        elif c == '=':
            # Check if it's '==' (equality) or just '=' (assignment)
            if self.match('='):
                self.add_token(TokenType.EQUAL_EQUAL)
            else:
                self.add_token(TokenType.EQUAL)
        elif c == '<':
            # Check if it's '<=' (less than or equal to) or just '<' (less than)
            if self.match('='):
                self.add_token(TokenType.LESS_EQUAL)
            else:
                self.add_token(TokenType.LESS)
        elif c == '>':
            # Check if it's '>=' (greater than or equal to) or just '>' (greater than)
            if self.match('='):
                self.add_token(TokenType.GREATER_EQUAL)
            else:
                self.add_token(TokenType.GREATER)
        # String literals
        elif c == '"':
            self.string()
        # Number literals
        elif c.isdigit():
            self.number()
        # Handle whitespace characters
        elif c.isspace():
            # If newline, increment line counter
            if c == '\n':
                self.line += 1
        # Identifiers and keywords
        elif self.is_alpha(c):
            self.identifier()
        else:
            # Report error for unexpected characters
            self.error(c)
    
    def identifier(self):
        """Process an identifier or keyword."""
        while not self.is_at_end() and self.is_alphanumeric(self.peek()):
            self.advance()
        
        # Get the identifier text
        text = self.source[self.start:self.current]
        
        # Check if it's a reserved keyword
        token_type = self.keywords.get(text, TokenType.IDENTIFIER)
        
        # Add the token
        self.add_token(token_type)
    
    def is_alpha(self, c):
        """Check if a character is a letter or underscore."""
        return c.isalpha() or c == '_'
    
    def is_alphanumeric(self, c):
        """Check if a character is a letter, digit, or underscore."""
        return self.is_alpha(c) or c.isdigit()
    
    def string(self):
        """Process a string literal."""
        # Consume characters until closing quote or end of file
        while not self.is_at_end() and self.peek() != '"':
            if self.peek() == '\n':
                self.line += 1
            self.advance()
        
        # Check for unterminated string
        if self.is_at_end():
            print(f"[line {self.line}] Error: Unterminated string.", file=sys.stderr)
            self.had_error = True
            return
            
        # Consume the closing "
        self.advance()
        
        # Extract the string value (without the surrounding quotes)
        value = self.source[self.start + 1:self.current - 1]
        self.add_token(TokenType.STRING, value)
    
    def number(self):
        """Process a number literal."""
        # Consume digits
        while not self.is_at_end() and self.peek().isdigit():
            self.advance()
            
        # Look for a fractional part
        if not self.is_at_end() and self.peek() == '.' and self.peek_next().isdigit():
            # Consume the "."
            self.advance()
            
            # Consume fractional digits
            while not self.is_at_end() and self.peek().isdigit():
                self.advance()
        
        # Convert to float and add token
        value = float(self.source[self.start:self.current])
        self.add_token(TokenType.NUMBER, value)
    
    def peek_next(self):
        """Look at the character two positions ahead without consuming it."""
        if self.current + 1 >= len(self.source):
            return '\0'
        return self.source[self.current + 1]
    
    def match(self, expected):
        """Conditionally consume the next character if it matches expected."""
        if self.is_at_end():
            return False
        if self.source[self.current] != expected:
            return False
        
        self.current += 1
        return True
    
    def peek(self):
        """Look at the current character without consuming it."""
        if self.is_at_end():
            return '\0'
        return self.source[self.current]
    
    def error(self, character):
        """Report a lexical error."""
        print(f"[line {self.line}] Error: Unexpected character: {character}", file=sys.stderr)
        self.had_error = True
    
    def add_token(self, token_type, literal=None):
        """Add a token to the list."""
        lexeme = self.source[self.start:self.current]
        self.tokens.append(Token(token_type, lexeme, literal, self.line))
    
    def advance(self):
        """Consume the next character and return it."""
        self.current += 1
        return self.source[self.current - 1]
    
    def is_at_end(self):
        """Check if we've reached the end of the source."""
        return self.current >= len(self.source)

# Interpreter for evaluating expressions
class Interpreter:
    """Evaluates expressions and returns their values."""
    
    def __init__(self):
        self.had_runtime_error = False
    
    def interpret(self, expr):
        """Interpret an expression and return its value."""
        try:
            value = self.evaluate(expr)
            return self.stringify(value)
        except LoxRuntimeError as error:
            # Handle runtime errors
            self.had_runtime_error = True
            print(f"{error.message}\n[line {error.token.line}]", file=sys.stderr)
            return None
        except Exception as error:
            # Handle other unexpected errors
            self.had_runtime_error = True
            print(f"Runtime Error: {error}", file=sys.stderr)
            return None
    
    def evaluate(self, expr):
        """Evaluate an expression and return its value."""
        return expr.accept(self)
    
    def visit_literal_expr(self, expr):
        """Evaluate a literal expression."""
        return expr.value
    
    def visit_grouping_expr(self, expr):
        """Evaluate a grouping expression."""
        return self.evaluate(expr.expression)
    
    def visit_unary_expr(self, expr):
        """Evaluate a unary expression."""
        # Evaluate the operand first
        right = self.evaluate(expr.right)
        
        # Handle different unary operators
        if expr.operator.token_type == TokenType.MINUS:
            # Negation operator: ensure operand is a number
            self.check_number_operand(expr.operator, right)
            return -float(right)
        elif expr.operator.token_type == TokenType.BANG:
            # Logical NOT operator: invert truthiness
            return not self.is_truthy(right)
        
        # Unreachable
        return None
    
    def visit_binary_expr(self, expr):
        """Evaluate a binary expression."""
        # Evaluate both operands
        left = self.evaluate(expr.left)
        right = self.evaluate(expr.right)
        
        # Handle different binary operators
        if expr.operator.token_type == TokenType.EQUAL_EQUAL:
            # Equality
            return self.is_equal(left, right)
        elif expr.operator.token_type == TokenType.BANG_EQUAL:
            # Inequality
            return not self.is_equal(left, right)
        elif expr.operator.token_type == TokenType.GREATER:
            # Greater than
            self.check_number_operands(expr.operator, left, right)
            return float(left) > float(right)
        elif expr.operator.token_type == TokenType.GREATER_EQUAL:
            # Greater than or equal
            self.check_number_operands(expr.operator, left, right)
            return float(left) >= float(right)
        elif expr.operator.token_type == TokenType.LESS:
            # Less than
            self.check_number_operands(expr.operator, left, right)
            return float(left) < float(right)
        elif expr.operator.token_type == TokenType.LESS_EQUAL:
            # Less than or equal
            self.check_number_operands(expr.operator, left, right)
            return float(left) <= float(right)
        elif expr.operator.token_type == TokenType.STAR:
            # Multiplication
            self.check_number_operands(expr.operator, left, right)
            return float(left) * float(right)
        elif expr.operator.token_type == TokenType.SLASH:
            # Division
            self.check_number_operands(expr.operator, left, right)
            return float(left) / float(right)
        elif expr.operator.token_type == TokenType.PLUS:
            # Addition or string concatenation
            if isinstance(left, str) and isinstance(right, str):
                # String concatenation
                return left + right
            
            if isinstance(left, (int, float)) and isinstance(right, (int, float)):
                # Numeric addition
                return float(left) + float(right)
            
            # For now, we'll assume no error cases as per the stage description
            self.check_number_operands(expr.operator, left, right)
            return float(left) + float(right)
        elif expr.operator.token_type == TokenType.MINUS:
            # Subtraction
            self.check_number_operands(expr.operator, left, right)
            return float(left) - float(right)
        
        # Placeholder for other binary operators to be implemented later
        raise NotImplementedError(f"Binary operator {expr.operator.lexeme} not yet implemented")

    def is_equal(self, a, b):
        """Check if two values are equal according to Lox rules."""
        # nil is only equal to nil
        if a is None and b is None:
            return True
        if a is None:
            return False
        
        # In Lox, values of different types are never equal
        if type(a) != type(b):
            return False
        
        # Compare the values
        return a == b

    def check_number_operands(self, operator, left, right):
        """Verify that both operands are numbers."""
        if isinstance(left, (int, float)) and isinstance(right, (int, float)):
            return
        raise LoxRuntimeError(operator, "Operands must be numbers.")
    
    def is_truthy(self, value):
        """
        Determine truthiness according to Lox rules:
        - false and nil are falsy
        - everything else is truthy
        """
        if value is None:  # nil
            return False
        if isinstance(value, bool):  # boolean
            return value
        # Everything else is truthy
        return True
    
    def check_number_operand(self, operator, operand):
        """Verify that an operand is a number."""
        if isinstance(operand, (int, float)):
            return
        raise LoxRuntimeError(operator, "Operand must be a number.")
    
    def stringify(self, value):
        """Convert a value to its string representation."""
        if value is None:
            return "nil"
        if isinstance(value, bool):
            return str(value).lower()
        if isinstance(value, float):
            # Remove trailing .0 for integers
            text = str(value)
            if text.endswith(".0"):
                text = text[:-2]
            return text
        # For strings and other types
        return str(value)

def main():
    if len(sys.argv) < 3:
        print("Usage: ./your_program.sh [tokenize|parse|evaluate] <filename>", file=sys.stderr)
        exit(1)

    command = sys.argv[1]
    filename = sys.argv[2]

    if command not in ["tokenize", "parse", "evaluate"]:
        print(f"Unknown command: {command}", file=sys.stderr)
        exit(1)

    with open(filename) as file:
        file_contents = file.read()

    # You can use print statements as follows for debugging, they'll be visible when running tests.
    print("Logs from your program will appear here!", file=sys.stderr)

    # Scan tokens
    scanner = Scanner(file_contents)
    tokens = scanner.scan_tokens()
    
    # Handle different commands
    if command == "tokenize":
        # Print tokens in the required format
        for token in tokens:
            print(token)
    elif command == "parse":
        # Parse the tokens into an AST and print it
        parser = Parser(tokens)
        expression = parser.parse()
        if expression:
            printer = AstPrinter()
            print(printer.print(expression))
        else:
            print("Error: Failed to parse expression.", file=sys.stderr)
            exit(65)
    elif command == "evaluate":
        # Parse and evaluate the expression
        parser = Parser(tokens)
        expression = parser.parse()
        if expression:
            interpreter = Interpreter()
            result = interpreter.interpret(expression)
            if interpreter.had_runtime_error:
                exit(70)  # Runtime error
            if result is not None:
                print(result)
        else:
            print("Error: Failed to parse expression.", file=sys.stderr)
            exit(65)
    
    # Exit with code 65 if there were lexical errors
    if scanner.had_error:
        exit(65)
    
    # Exit with code 65 if there were parsing errors
    if (command == "parse" or command == "evaluate") and parser.had_error:
        exit(65)


if __name__ == "__main__":
    main()
