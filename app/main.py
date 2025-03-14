import sys
from enum import Enum

class TokenType(Enum):
    # Single-character tokens
    LEFT_PAREN = "LEFT_PAREN"
    RIGHT_PAREN = "RIGHT_PAREN"
    LEFT_BRACE = "LEFT_BRACE"
    RIGHT_BRACE = "RIGHT_BRACE"
    COMMA = "COMMA"        # New token type for ','
    DOT = "DOT"            # New token type for '.'
    MINUS = "MINUS"        # New token type for '-'
    PLUS = "PLUS"          # New token type for '+'
    SEMICOLON = "SEMICOLON"  # New token type for ';'
    STAR = "STAR"          # New token type for '*'
    
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
        # Ignore whitespace characters
        elif c.isspace():
            pass
        else:
            # Report error for unexpected characters
            self.error(c)
    
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
