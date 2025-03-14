import sys
from enum import Enum

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
    NUMBER = "NUMBER"        # New token type for number literals
    
    # End of file
    EOF = "EOF"

class Token:
    def __init__(self, token_type, lexeme, literal=None):
        self.token_type = token_type
        self.lexeme = lexeme
        self.literal = literal
    
    def __str__(self):
        return f"{self.token_type.value} {self.lexeme} {self.literal if self.literal is not None else 'null'}"

class Scanner:
    def __init__(self, source):
        self.source = source
        self.tokens = []
        self.start = 0
        self.current = 0
        self.line = 1  # Track line number for error reporting
        self.had_error = False  # Track if any errors occurred
    
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
        else:
            # Report error for unexpected characters
            self.error(c)
    
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
        self.tokens.append(Token(token_type, lexeme, literal))
    
    def advance(self):
        """Consume the next character and return it."""
        self.current += 1
        return self.source[self.current - 1]
    
    def is_at_end(self):
        """Check if we've reached the end of the source."""
        return self.current >= len(self.source)

def main():
    if len(sys.argv) < 3:
        print("Usage: ./your_program.sh tokenize <filename>", file=sys.stderr)
        exit(1)

    command = sys.argv[1]
    filename = sys.argv[2]

    if command != "tokenize":
        print(f"Unknown command: {command}", file=sys.stderr)
        exit(1)

    with open(filename) as file:
        file_contents = file.read()

    # You can use print statements as follows for debugging, they'll be visible when running tests.
    print("Logs from your program will appear here!", file=sys.stderr)

    # Scan tokens
    scanner = Scanner(file_contents)
    tokens = scanner.scan_tokens()
    
    # Print tokens in the required format
    for token in tokens:
        print(token)
    
    # Exit with code 65 if there were lexical errors
    if scanner.had_error:
        exit(65)


if __name__ == "__main__":
    main()
