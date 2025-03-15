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

class Variable(Expr):
    """Variable reference expression."""
    def __init__(self, name):
        self.name = name  # Token
    
    def accept(self, visitor):
        return visitor.visit_variable_expr(self)

# 1. Add the Assign expression class
class Assign(Expr):
    """Assignment expression."""
    def __init__(self, name, value):
        self.name = name  # Token for variable name
        self.value = value  # Expression for value being assigned
    
    def accept(self, visitor):
        return visitor.visit_assign_expr(self)

# 1. Add Logical expression class
class Logical(Expr):
    """Logical expression with two operands and a logical operator."""
    def __init__(self, left, operator, right):
        self.left = left        # Left expression
        self.operator = operator  # Token (OR or AND)
        self.right = right      # Right expression
    
    def accept(self, visitor):
        return visitor.visit_logical_expr(self)

# Create a Call expression class
class Call(Expr):
    """Function call expression."""
    def __init__(self, callee, paren, arguments):
        self.callee = callee      # Expression to evaluate to a function
        self.paren = paren        # Token for error reporting
        self.arguments = arguments  # List of argument expressions
    
    def accept(self, visitor):
        return visitor.visit_call_expr(self)

# AST classes for statements
class Stmt:
    """Base class for all statements."""
    pass

class Expression(Stmt):
    """Statement that evaluates an expression."""
    def __init__(self, expression):
        self.expression = expression
    
    def accept(self, visitor):
        return visitor.visit_expression_stmt(self)

class Print(Stmt):
    """Print statement."""
    def __init__(self, expression):
        self.expression = expression
    
    def accept(self, visitor):
        return visitor.visit_print_stmt(self)

class Var(Stmt):
    """Variable declaration statement."""
    def __init__(self, name, initializer):
        self.name = name  # Token
        self.initializer = initializer  # Expression
    
    def accept(self, visitor):
        return visitor.visit_var_stmt(self)

# 1. Add Block statement class
class Block(Stmt):
    """Block statement containing a list of statements."""
    def __init__(self, statements):
        self.statements = statements  # List of statements
    
    def accept(self, visitor):
        return visitor.visit_block_stmt(self)

# Add If statement class
class If(Stmt):
    """If statement with condition and branches."""
    def __init__(self, condition, then_branch, else_branch=None):
        self.condition = condition  # Expression to evaluate
        self.then_branch = then_branch  # Statement to execute if condition is true
        self.else_branch = else_branch  # Optional statement to execute if condition is false
    
    def accept(self, visitor):
        return visitor.visit_if_stmt(self)

# Add While statement class
class While(Stmt):
    """While loop statement."""
    def __init__(self, condition, body):
        self.condition = condition  # Expression to evaluate before each iteration
        self.body = body  # Statement to execute in each iteration
    
    def accept(self, visitor):
        return visitor.visit_while_stmt(self)

# 1. Add Function declaration statement class
class Function(Stmt):
    """Function declaration statement."""
    def __init__(self, name, params, body):
        self.name = name        # Token for function name
        self.params = params    # List of tokens for parameter names
        self.body = body        # List of statements for function body
    
    def accept(self, visitor):
        return visitor.visit_function_stmt(self)

# Add Return statement class
class Return(Stmt):
    """Return statement."""
    def __init__(self, keyword, value=None):
        self.keyword = keyword  # Token for 'return'
        self.value = value      # Expression for return value (or None)
    
    def accept(self, visitor):
        return visitor.visit_return_stmt(self)

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

# Update the Parser to handle statements
class Parser:
    """Parses tokens into statements."""
    
    def __init__(self, tokens):
        self.tokens = tokens
        self.current = 0
        self.had_error = False
    
    def parse(self):
        """Parse tokens into a list of statements."""
        statements = []
        while not self.is_at_end():
            try:
                statements.append(self.declaration())
            except Exception as error:
                # Report parser error and synchronize
                self.had_error = True
                print(f"Parse error: {error}", file=sys.stderr)
                self.synchronize()  # Skip to next statement boundary
        return statements
    
    def synchronize(self):
        """Recover from a parsing error by skipping tokens until next statement."""
        self.advance()  # Consume the erroneous token
        
        while not self.is_at_end():
            # If we reach a statement boundary, return
            if self.previous().token_type == TokenType.SEMICOLON:
                return
            
            # Look for tokens that might start a new statement
            if self.peek().token_type in [
                TokenType.CLASS, TokenType.FUN, TokenType.VAR, 
                TokenType.FOR, TokenType.IF, TokenType.WHILE,
                TokenType.PRINT, TokenType.RETURN
            ]:
                return
            
            self.advance()
    
    def statement(self):
        """Parse a statement."""
        if self.match(TokenType.FOR):
            return self.for_statement()
        
        if self.match(TokenType.IF):
            return self.if_statement()
        
        if self.match(TokenType.PRINT):
            return self.print_statement()
        
        if self.match(TokenType.RETURN):
            return self.return_statement()
        
        if self.match(TokenType.WHILE):
            return self.while_statement()
        
        if self.match(TokenType.LEFT_BRACE):
            return Block(self.block())
        
        return self.expression_statement()

    def return_statement(self):
        """Parse a return statement."""
        keyword = self.previous()  # The 'return' token
        
        # Check for return value
        value = None
        if not self.check(TokenType.SEMICOLON):
            value = self.expression()
        
        self.consume(TokenType.SEMICOLON, "Expect ';' after return value.")
        return Return(keyword, value)

    def for_statement(self):
        """Parse a for statement by desugaring it to a while loop."""
        # Consume the opening parenthesis
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")
        
        # Parse initializer
        initializer = None
        if self.match(TokenType.SEMICOLON):
            # No initializer
            initializer = None
        elif self.match(TokenType.VAR):
            # Variable declaration initializer
            initializer = self.var_declaration()
        else:
            # Expression initializer
            initializer = self.expression_statement()
        
        # Parse condition
        condition = None
        if not self.check(TokenType.SEMICOLON):
            condition = self.expression()
        self.consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")
        
        # Parse increment
        increment = None
        if not self.check(TokenType.RIGHT_PAREN):
            increment = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")
        
        # Parse body
        body = self.statement()
        
        # Transform the for loop into a while loop
        
        # If there's an increment, append it to the body
        if increment is not None:
            # Create a block containing the original body and the increment expression
            body = Block([body, Expression(increment)])
        
        # If condition was omitted, use true as default (infinite loop)
        if condition is None:
            condition = Literal(True)
        
        # Create the while loop
        body = While(condition, body)
        
        # If there's an initializer, execute it before the while loop
        if initializer is not None:
            body = Block([initializer, body])
        
        return body

    def if_statement(self):
        """Parse an if statement."""
        # Consume the opening parenthesis after 'if'
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
        
        # Parse the condition expression
        condition = self.expression()
        
        # Consume the closing parenthesis
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")
        
        # Parse the then-branch statement
        then_branch = self.statement()
        
        # Check for optional else-branch
        else_branch = None
        if self.match(TokenType.ELSE):
            else_branch = self.statement()
        
        return If(condition, then_branch, else_branch)

    def block(self):
        """Parse a block of statements."""
        statements = []
        
        while not self.check(TokenType.RIGHT_BRACE) and not self.is_at_end():
            statements.append(self.declaration())
        
        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
        return statements
    
    def print_statement(self):
        """Parse a print statement."""
        value = self.expression()
        self.consume(TokenType.SEMICOLON, "Expect ';' after value.")
        return Print(value)
    
    def expression_statement(self):
        """Parse an expression statement."""
        expr = self.expression()
        self.consume(TokenType.SEMICOLON, "Expect ';' after expression.")
        return Expression(expr)
    
    # 3. Update the Parser to handle assignment expressions
    def expression(self):
        """Parse an expression."""
        return self.assignment()  # Start with assignment now

    def assignment(self):
        """Parse an assignment expression."""
        expr = self.logic_or()  # Changed from self.equality()
        
        if self.match(TokenType.EQUAL):
            equals = self.previous()
            # Recursively parse the right side (for right associativity)
            value = self.assignment()
            
            # If the left side is a variable, create an assignment
            if isinstance(expr, Variable):
                name = expr.name
                return Assign(name, value)
            
            # Otherwise, it's an invalid assignment target
            raise Exception("Invalid assignment target.")
        
        return expr

    # Update Parser to handle logical AND expressions
    def logic_or(self):
        """Parse a logical OR expression."""
        expr = self.logic_and()  # Changed from self.equality()
        
        while self.match(TokenType.OR):
            operator = self.previous()
            right = self.logic_and()  # Changed from self.equality()
            expr = Logical(expr, operator, right)
        
        return expr

    def logic_and(self):
        """Parse a logical AND expression."""
        expr = self.equality()
        
        while self.match(TokenType.AND):
            operator = self.previous()
            right = self.equality()
            expr = Logical(expr, operator, right)
        
        return expr

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
        
        # If no unary operator, parse as a call expression
        return self.call()  # Changed from self.primary()

    def call(self):
        """Parse a function call."""
        expr = self.primary()
        
        while True:
            if self.match(TokenType.LEFT_PAREN):
                expr = self.finish_call(expr)
            else:
                break
        
        return expr

    def finish_call(self, callee):
        """Parse the arguments of a function call."""
        arguments = []
        
        if not self.check(TokenType.RIGHT_PAREN):
            # Parse arguments
            arguments.append(self.expression())
            while self.match(TokenType.COMMA):
                if len(arguments) >= 255:
                    raise Exception("Can't have more than 255 arguments.")
                arguments.append(self.expression())
        
        paren = self.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")
        
        return Call(callee, paren, arguments)

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
        
        # Add this to handle variable references
        if self.match(TokenType.IDENTIFIER):
            return Variable(self.previous())
        
        # Rest of your primary method...
        
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

    def declaration(self):
        """Parse a declaration."""
        try:
            if self.match(TokenType.FUN):
                return self.function("function")
            if self.match(TokenType.VAR):
                return self.var_declaration()
            
            return self.statement()
        except Exception as error:
            # Report parser error and synchronize
            self.had_error = True
            print(f"Parse error: {error}", file=sys.stderr)
            self.synchronize()  # Skip to next statement boundary
            return None

    def function(self, kind):
        """Parse a function declaration."""
        # Get the function name
        name = self.consume(TokenType.IDENTIFIER, f"Expect {kind} name.")
        
        # Parse parameters
        self.consume(TokenType.LEFT_PAREN, f"Expect '(' after {kind} name.")
        parameters = []
        
        # Parse parameter list
        if not self.check(TokenType.RIGHT_PAREN):
            # Parse first parameter
            parameters.append(self.consume(TokenType.IDENTIFIER, "Expect parameter name."))
            
            # Parse additional parameters (comma-separated)
            while self.match(TokenType.COMMA):
                if len(parameters) >= 255:
                    raise Exception("Can't have more than 255 parameters.")
                
                parameters.append(self.consume(TokenType.IDENTIFIER, "Expect parameter name."))
        
        self.consume(TokenType.RIGHT_PAREN, f"Expect ')' after parameters.")
        
        # Parse the function body
        self.consume(TokenType.LEFT_BRACE, f"Expect '{{' before {kind} body.")
        body = self.block()
        
        return Function(name, parameters, body)

    def var_declaration(self):
        """Parse a variable declaration."""
        name = self.consume(TokenType.IDENTIFIER, "Expect variable name.")
        
        # Check for initializer
        initializer = None
        if self.match(TokenType.EQUAL):
            initializer = self.expression()
        
        self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
        return Var(name, initializer)

    def while_statement(self):
        """Parse a while statement."""
        # Consume the opening parenthesis after 'while'
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
        
        # Parse the condition expression
        condition = self.expression()
        
        # Consume the closing parenthesis
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")
        
        # Parse the body statement
        body = self.statement()
        
        return While(condition, body)

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

# Add a callable interface
class LoxCallable:
    """Interface for callable objects in Lox."""
    def call(self, interpreter, arguments):
        """Call the function with the given arguments."""
        pass

    def arity(self):
        """Return the number of arguments this function expects."""
        pass

# Update the Interpreter to handle statements
class Interpreter:
    """Evaluates expressions and executes statements."""
    
    def __init__(self):
        self.had_runtime_error = False
        self.environment = Environment()
        self.globals = self.environment  # Reference to global environment
        self.locals = {}  # Map expressions to their resolved depths
        
        # Define native clock function
        self.environment.define("clock", NativeFunction(lambda _: time.time(), 0))
    
    def interpret(self, statements):
        """Interpret a list of statements."""
        try:
            for stmt in statements:
                self.execute(stmt)
        except LoxRuntimeError as error:
            # Handle runtime errors
            self.had_runtime_error = True
            print(f"{error.message}\n[line {error.token.line}]", file=sys.stderr)
        except Exception as error:
            # Handle other unexpected errors
            self.had_runtime_error = True
            print(f"Runtime Error: {error}", file=sys.stderr)
    
    def execute(self, stmt):
        """Execute a statement."""
        stmt.accept(self)
    
    def visit_expression_stmt(self, stmt):
        """Execute an expression statement."""
        self.evaluate(stmt.expression)
        return None
    
    def visit_print_stmt(self, stmt):
        """Execute a print statement."""
        value = self.evaluate(stmt.expression)
        print(self.stringify(value))
        return None
    
    def visit_var_stmt(self, stmt):
        """Execute a variable declaration statement."""
        value = None
        if stmt.initializer is not None:
            value = self.evaluate(stmt.initializer)
        
        self.environment.define(stmt.name.lexeme, value)
        return None

    def visit_variable_expr(self, expr):
        """Evaluate a variable reference expression."""
        return self.lookup_variable(expr.name, expr)
    
    def lookup_variable(self, name, expr):
        """Look up a variable using static resolution information if available."""
        if expr in self.locals:
            distance = self.locals[expr]
            return self.environment.get_at(distance, name.lexeme)
        else:
            # Not resolved locally, must be global
            return self.globals.get(name)
    
    # 4. Add the interpreter method for assignment expressions
    def visit_assign_expr(self, expr):
        """Evaluate an assignment expression."""
        value = self.evaluate(expr.value)
        
        if expr in self.locals:
            distance = self.locals[expr]
            self.environment.assign_at(distance, expr.name, value)
        else:
            # Global variable
            self.globals.assign(expr.name, value)
        
        return value
    
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
            # Check if operands are numbers and have same type
            if type(left) != type(right):
                raise LoxRuntimeError(expr.operator, "Operands must be numbers of the same type.")
            self.check_number_operands(expr.operator, left, right)
            return float(left) > float(right)
        elif expr.operator.token_type == TokenType.GREATER_EQUAL:
            # Greater than or equal
            # Check if operands are numbers and have same type
            if type(left) != type(right):
                raise LoxRuntimeError(expr.operator, "Operands must be numbers of the same type.")
            self.check_number_operands(expr.operator, left, right)
            return float(left) >= float(right)
        elif expr.operator.token_type == TokenType.LESS:
            # Less than
            # Check if operands are numbers and have same type
            if type(left) != type(right):
                raise LoxRuntimeError(expr.operator, "Operands must be numbers of the same type.")
            self.check_number_operands(expr.operator, left, right)
            return float(left) < float(right)
        elif expr.operator.token_type == TokenType.LESS_EQUAL:
            # Less than or equal
            # Check if operands are numbers and have same type
            if type(left) != type(right):
                raise LoxRuntimeError(expr.operator, "Operands must be numbers of the same type.")
            self.check_number_operands(expr.operator, left, right)
            return float(left) <= float(right)
        elif expr.operator.token_type == TokenType.PLUS:
            # Addition or string concatenation
            if isinstance(left, str) and isinstance(right, str):
                # String concatenation
                return left + right
            
            if isinstance(left, (int, float)) and isinstance(right, (int, float)):
                # Numeric addition
                return float(left) + float(right)
            
            # For now, we'll assume no error cases as per previous stage descriptions
            raise LoxRuntimeError(expr.operator, "Operands must be two numbers or two strings.")
        elif expr.operator.token_type == TokenType.MINUS:
            # Subtraction
            self.check_number_operands(expr.operator, left, right)
            return float(left) - float(right)
        elif expr.operator.token_type == TokenType.STAR:
            # Multiplication
            self.check_number_operands(expr.operator, left, right)
            return float(left) * float(right)
        elif expr.operator.token_type == TokenType.SLASH:
            # Division
            self.check_number_operands(expr.operator, left, right)
            return float(left) / float(right)
        
        # Should never reach here
        raise LoxRuntimeError(expr.operator, f"Unknown operator: {expr.operator.lexeme}")

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
        # Explicitly reject booleans
        if isinstance(left, bool) or isinstance(right, bool):
            raise LoxRuntimeError(operator, "Operands must be numbers.")
        # Then check if they're numbers
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
        # Explicitly reject booleans
        if isinstance(operand, bool):
            raise LoxRuntimeError(operator, "Operand must be a number.")
        # Then check if it's a number
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
        if isinstance(value, LoxCallable):
            # For callable objects, use their string representation
            return str(value)
        # For strings and other types
        return str(value)

    def visit_block_stmt(self, stmt):
        """Execute a block statement."""
        # Execute the block with a new environment
        self.execute_block(stmt.statements, Environment(self.environment))
        return None

    def execute_block(self, statements, environment):
        """Execute a list of statements in the given environment."""
        # Save the previous environment
        previous = self.environment
        
        try:
            # Update the current environment
            self.environment = environment
            
            # Execute all statements in the block
            for statement in statements:
                self.execute(statement)
        finally:
            # Restore the previous environment when done
            self.environment = previous

    def visit_if_stmt(self, stmt):
        """Execute an if statement."""
        # Evaluate the condition
        condition = self.evaluate(stmt.condition)
        
        # Check if the condition is truthy
        if self.is_truthy(condition):
            # Execute the then-branch
            self.execute(stmt.then_branch)
        elif stmt.else_branch is not None:
            # Execute the else-branch if it exists
            self.execute(stmt.else_branch)
        
        return None

    # 3. Add interpreter method for logical expressions
    def visit_logical_expr(self, expr):
        """Evaluate a logical expression."""
        left = self.evaluate(expr.left)
        
        # Short-circuit evaluation for OR
        if expr.operator.token_type == TokenType.OR:
            # If left side is truthy, return it without evaluating right side
            if self.is_truthy(left):
                return left
        # Short-circuit evaluation for AND    
        elif expr.operator.token_type == TokenType.AND:
            # If left side is falsy, return it without evaluating right side
            if not self.is_truthy(left):
                return left
        
        # For OR: if left side is falsy, evaluate and return right side
        # For AND: if left side is truthy, evaluate and return right side
        return self.evaluate(expr.right)

    def visit_while_stmt(self, stmt):
        """Execute a while statement."""
        # Keep executing the body as long as the condition is truthy
        while self.is_truthy(self.evaluate(stmt.condition)):
            self.execute(stmt.body)
        
        return None

    def visit_call_expr(self, expr):
        """Evaluate a function call expression."""
        callee = self.evaluate(expr.callee)
        
        arguments = []
        for argument in expr.arguments:
            arguments.append(self.evaluate(argument))
        
        # Check if callee is callable
        if not isinstance(callee, LoxCallable):
            raise LoxRuntimeError(expr.paren, "Can only call functions and classes.")
        
        # Check argument count
        if len(arguments) != callee.arity():
            raise LoxRuntimeError(expr.paren, 
                               f"Expected {callee.arity()} arguments but got {len(arguments)}.")
        
        # Call the function
        return callee.call(self, arguments)

    def visit_function_stmt(self, stmt):
        """Execute a function declaration statement."""
        # Create a function object with the current environment as closure
        function = LoxFunction(stmt, self.environment)
        
        # Define the function in the current environment
        self.environment.define(stmt.name.lexeme, function)
        
        return None

    def visit_return_stmt(self, stmt):
        """Execute a return statement."""
        value = None
        if stmt.value is not None:
            value = self.evaluate(stmt.value)
        
        # Using an exception for control flow to return early from nested calls
        raise ReturnException(value)
    
    def resolve(self, expr, depth):
        """Store the resolution information for an expression."""
        self.locals[expr] = depth

# 2. LoxFunction class for runtime function objects
class LoxFunction(LoxCallable):
    """A user-defined function."""
    
    def __init__(self, declaration, closure):
        self.declaration = declaration  # Function declaration AST node
        self.closure = closure          # Environment where function was defined
    
    def call(self, interpreter, arguments):
        # Create a new environment for the function execution
        environment = Environment(self.closure)
        
        # Bind parameters to arguments
        for i, parameter in enumerate(self.declaration.params):
            environment.define(parameter.lexeme, arguments[i])
        
        try:
            # Execute the function body in the new environment
            interpreter.execute_block(self.declaration.body, environment)
        except ReturnException as return_value:
            # Return the value from the return statement
            return return_value.value
        
        # Functions without explicit returns return nil
        return None
    
    def arity(self):
        # Return the number of parameters
        return len(self.declaration.params)
    
    def __str__(self):
        return f"<fn {self.declaration.name.lexeme}>"

# Custom exception for handling return values
class ReturnException(Exception):
    """Exception for handling return values from functions."""
    def __init__(self, value):
        self.value = value
        super().__init__(str(value))

# Update the Environment class to support nesting
class Environment:
    """Environment for storing variable bindings."""
    
    def __init__(self, enclosing=None):
        self.values = {}
        self.enclosing = enclosing  # Reference to the outer environment
    
    def define(self, name, value):
        """Define a new variable with the given name and value."""
        self.values[name] = value
    
    def get(self, name):
        """Get the value of a variable."""
        if name.lexeme in self.values:
            return self.values[name.lexeme]
            
        # If not found in this environment, look in the enclosing one
        if self.enclosing:
            return self.enclosing.get(name)
        
        raise LoxRuntimeError(name, f"Undefined variable '{name.lexeme}'.")
    
    def set(self, name, value):
        """Set the value of an existing variable."""
        if name.lexeme in self.values:
            self.values[name.lexeme] = value
            return value
        
        # If not found in this environment, try the enclosing one
        if self.enclosing:
            return self.enclosing.set(name, value)
            
        raise LoxRuntimeError(name, f"Undefined variable '{name.lexeme}'.")

    def get_at(self, distance, name):
        """Get the value of a variable at a specific distance."""
        return self.ancestor(distance).values.get(name)

    def assign_at(self, distance, name, value):
        """Assign a value to a variable at a specific distance."""
        self.ancestor(distance).values[name.lexeme] = value

    def ancestor(self, distance):
        """Get the environment at a specific distance."""
        environment = self
        for _ in range(distance):
            environment = environment.enclosing
        return environment

import time

# Implement native functions
class NativeFunction(LoxCallable):
    """A native function implemented in Python."""
    def __init__(self, function, arity):
        self.function = function
        self._arity = arity
        
    def call(self, interpreter, arguments):
        return self.function(arguments)
        
    def arity(self):
        return self._arity
        
    def __str__(self):
        return "<native fn>"

# Add these enums for tracking function and class context
class FunctionType(Enum):
    NONE = 0
    FUNCTION = 1

class Resolver:
    """Performs static analysis on the AST to resolve variable bindings."""
    
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.scopes = []  # Stack of scopes
        self.current_function = FunctionType.NONE
    
    def resolve(self, statements_or_expr):
        """Resolve variable bindings in statements or expression."""
        if isinstance(statements_or_expr, list):
            for statement in statements_or_expr:
                self.resolve(statement)
        else:
            statements_or_expr.accept(self)
    
    def begin_scope(self):
        """Start a new scope."""
        self.scopes.append({})
    
    def end_scope(self):
        """End the current scope."""
        self.scopes.pop()
    
    def declare(self, name):
        """Declare a variable in the current scope."""
        if not self.scopes:
            return  # Skip for global scope
        
        scope = self.scopes[-1]
        if name.lexeme in scope:
            print(f"Variable {name.lexeme} already declared in this scope.", file=sys.stderr)
        
        # Mark as declared but not defined
        scope[name.lexeme] = False
    
    def define(self, name):
        """Define a variable (mark it as initialized)."""
        if not self.scopes:
            return  # Skip for global scope
        
        self.scopes[-1][name.lexeme] = True
    
    def resolve_local(self, expr, name):
        """Resolve a local variable reference."""
        # Search scopes from innermost to outermost
        for i in range(len(self.scopes) - 1, -1, -1):
            if name.lexeme in self.scopes[i]:
                # Tell interpreter how many scopes away this variable is
                self.interpreter.resolve(expr, len(self.scopes) - 1 - i)
                return
        
        # Not found, assume it's global
    
    def resolve_function(self, function, function_type):
        """Resolve a function declaration."""
        enclosing_function = self.current_function
        self.current_function = function_type
        
        self.begin_scope()
        
        # Declare and define each parameter
        for param in function.params:
            self.declare(param)
            self.define(param)
        
        # Resolve the function body
        self.resolve(function.body)
        
        self.end_scope()
        self.current_function = enclosing_function
    
    # Visitor methods for expressions
    def visit_binary_expr(self, expr):
        self.resolve(expr.left)
        self.resolve(expr.right)
    
    def visit_call_expr(self, expr):
        self.resolve(expr.callee)
        
        for argument in expr.arguments:
            self.resolve(argument)
    
    def visit_grouping_expr(self, expr):
        self.resolve(expr.expression)
    
    def visit_literal_expr(self, expr):
        # Nothing to resolve in literals
        pass
    
    def visit_logical_expr(self, expr):
        self.resolve(expr.left)
        self.resolve(expr.right)
    
    def visit_unary_expr(self, expr):
        self.resolve(expr.right)
    
    def visit_variable_expr(self, expr):
        # Check if we're trying to read a variable in its own initializer
        if self.scopes and expr.name.lexeme in self.scopes[-1] and self.scopes[-1][expr.name.lexeme] is False:
            print(f"Cannot read local variable {expr.name.lexeme} in its own initializer.", file=sys.stderr)
        
        self.resolve_local(expr, expr.name)
    
    def visit_assign_expr(self, expr):
        self.resolve(expr.value)
        self.resolve_local(expr, expr.name)
    
    # Visitor methods for statements
    def visit_block_stmt(self, stmt):
        self.begin_scope()
        self.resolve(stmt.statements)
        self.end_scope()
    
    def visit_expression_stmt(self, stmt):
        self.resolve(stmt.expression)
    
    def visit_function_stmt(self, stmt):
        # Declare and define the function name
        self.declare(stmt.name)
        self.define(stmt.name)
        
        self.resolve_function(stmt, FunctionType.FUNCTION)
    
    def visit_if_stmt(self, stmt):
        self.resolve(stmt.condition)
        self.resolve(stmt.then_branch)
        if stmt.else_branch:
            self.resolve(stmt.else_branch)
    
    def visit_print_stmt(self, stmt):
        self.resolve(stmt.expression)
    
    def visit_return_stmt(self, stmt):
        if self.current_function == FunctionType.NONE:
            print("Cannot return from top-level code.", file=sys.stderr)
        
        if stmt.value:
            self.resolve(stmt.value)
    
    def visit_var_stmt(self, stmt):
        self.declare(stmt.name)
        if stmt.initializer:
            self.resolve(stmt.initializer)
        self.define(stmt.name)
    
    def visit_while_stmt(self, stmt):
        self.resolve(stmt.condition)
        self.resolve(stmt.body)

# Update main function to support the 'run' command
def main():
    if len(sys.argv) < 3:
        print("Usage: ./your_program.sh [tokenize|parse|evaluate|run] <filename>", file=sys.stderr)
        exit(1)

    command = sys.argv[1]
    filename = sys.argv[2]

    if command not in ["tokenize", "parse", "evaluate", "run"]:
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
        # Only exit with error code after printing all valid tokens
        if scanner.had_error:
            exit(65)
    elif command == "parse":
        # Parse a single expression and print its AST representation
        parser = Parser(tokens)
        try:
            expression = parser.expression()
            # Don't check for EOF here - allow the expression to take up the entire input
            # parser.consume(TokenType.EOF, "Expect end of expression.")
            if expression:
                printer = AstPrinter()
                print(printer.print(expression))
            else:
                print("Error: Failed to parse expression.", file=sys.stderr)
                exit(65)
        except Exception as error:
            print(f"Parse error: {error}", file=sys.stderr)
            exit(65)
    elif command == "evaluate":
        try:
            # Parse the expression
            parser = Parser(tokens)
            expression = parser.expression()
            
            # Skip the EOF check that's causing the parse error
            # parser.consume(TokenType.EOF, "Expect end of expression.")
            
            # Evaluate the expression
            interpreter = Interpreter()
            value = interpreter.evaluate(expression)
            print(interpreter.stringify(value))
        except LoxRuntimeError as error:
            # This will catch our type mismatch for "bar" < true
            print(f"{error.message}", file=sys.stderr)
            exit(70)  # Return runtime error code
        except Exception as error:
            # Only catch parse errors if absolutely necessary
            print(f"Parse error: {error}", file=sys.stderr)
            exit(65)  # Syntax error
    elif command == "run":
        # Parse and execute a program
        parser = Parser(tokens)
        statements = parser.parse()
        if parser.had_error:
            exit(65)  # Syntax error
        
        # Create the interpreter
        interpreter = Interpreter()
        
        # Run the resolver
        resolver = Resolver(interpreter)
        try:
            resolver.resolve(statements)
        except Exception as error:
            print(f"Resolution error: {error}", file=sys.stderr)
            exit(65)
        
        # Run the interpreter
        interpreter.interpret(statements)
        if interpreter.had_runtime_error:
            exit(70)  # Runtime error


if __name__ == "__main__":
    main()
