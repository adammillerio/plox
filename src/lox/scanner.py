#!/usr/bin/env python3
from typing import Dict, List

from lox.lox import Lox
from lox.token import Token
from lox.token_type import TokenType


class Scanner:
    """Lox Scanner

    This class scans a given source text and returns a list of Tokens, to be
    used by the Parser to generate Statements.

    To use:
    Scanner("var a = 2;").scan_tokens()
    [TokenType.VAR var None,
     TokenType.IDENTIFIER a None,
     TokenType.EQUAL = None,
     TokenType.NUMBER 2 2.0,
     TokenType.SEMICOLON ; None,
     TokenType.EOF  None]

    Args:
        source: str. The lox source text to scan.

    Public Attributes:
        tokens: List[Token]. All scanned tokens.
        start: int. Start index in the source for the Token currently being scanned.
        current: int. The current index in the source, this will be combined with
            the start to generate the Token lexeme.
        line: int. Current line being scanned, this is incremented whenever a
            newline character is found in the source text.
    """

    # Used to map a scanned portion of the source text representing a keyword to
    # it's TokenType.
    keywords: Dict[str, TokenType] = {
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
        "while": TokenType.WHILE,
    }

    def __init__(self, source: str) -> None:
        self.source = source
        self.tokens: List[Token] = []
        self.start = 0
        self.current = 0
        self.line = 1

    def scan_tokens(self) -> List[Token]:
        """Scan the source text and return all scanned Tokens.

        This method will return regardless of whether or not there were errors
        during Scanning. Refer to Lox.error and Lox.had_error for more info.

        Returns:
            tokens: List[Token]. All successfully scanned Tokens.
        """

        while not self.is_at_end():
            # Move the start position up to the current index prior to scanning
            # the next token
            self.start = self.current
            self.scan_token()

        # Add the end-of-file token
        self.tokens.append(Token(TokenType.EOF, "", None, self.line))
        return self.tokens

    def is_at_end(self) -> bool:
        """Check if Scanner's current position is at the end of the sequence.

        Returns:
            at_end: bool. Whether or not the Scanner has reached the end of the
                sequence.
        """

        return self.current >= len(self.source)

    def scan_token(self) -> None:
        """Scan the remaining text for a Token.

        This is called at the start of a scan and after all token matches, until
        the end of the text is reached, adding a matched Token after each pass.
        """

        c = self.advance()
        match c:
            # Single character Lexemes.
            case "(":
                # Token(TokenType.LEFT_PAREN, "(", None, 1)
                self.add_empty_token(TokenType.LEFT_PAREN)
            case ")":
                # Token(TokenType.RIGHT_PAREN, ")", None, 1)
                self.add_empty_token(TokenType.RIGHT_PAREN)
            case "{":
                # Token(TokenType.LEFT_BRACE, "{", None, 1)
                self.add_empty_token(TokenType.LEFT_BRACE)
            case "}":
                # Token(TokenType.RIGHT_BRACE, "}", None, 1)
                self.add_empty_token(TokenType.RIGHT_BRACE)
            case ",":
                # Token(TokenType.COMMA, ",", None, 1)
                self.add_empty_token(TokenType.COMMA)
            case ".":
                # Token(TokenType.DOT, ".", None, 1)
                self.add_empty_token(TokenType.DOT)
            case "-":
                # Token(TokenType.MINUS, "-", None, 1)
                self.add_empty_token(TokenType.MINUS)
            case "+":
                # Token(TokenType.PLUS, "+", None, 1)
                self.add_empty_token(TokenType.PLUS)
            case ";":
                # Token(TokenType.SEMICOLON, ";", None, 1)
                self.add_empty_token(TokenType.SEMICOLON)
            case "*":
                # Token(TokenType.STAR, "*", None 1)
                self.add_empty_token(TokenType.STAR)
            # Two character Lexemes.
            case "!":
                # Token(TokenType.BANG,       "!", None 1)
                # Token(TokenType.BANG_EQUAL, "!=, None, 1)
                self.add_empty_token(
                    TokenType.BANG_EQUAL if self.match("=") else TokenType.BANG
                )
            case "=":
                # Token(TokenType.EQUAL,       "=", None, 1)
                # Token(TokenType.EQUAL_EQUAL, "==", None, 1)
                self.add_empty_token(
                    TokenType.EQUAL_EQUAL if self.match("=") else TokenType.EQUAL
                )
            case "<":
                # Token(TokenType.LESS,       "<", None, 1)
                # Token(TokenType.LESS_EQUAL, "<=", None, 1)
                self.add_empty_token(
                    TokenType.LESS_EQUAL if self.match("=") else TokenType.LESS
                )
            case ">":
                # Token(TokenType.GREATER,       ">", None, 1)
                # Token(TokenType.GREATER_EQUAL, ">=", None, 1)
                self.add_empty_token(
                    TokenType.GREATER_EQUAL if self.match("=") else TokenType.GREATER
                )
            # Division or Comment.
            case "/":
                # Second slash matched (//), line comment.
                if self.match("/"):
                    # A comment goes until the end of the line, so advance
                    # the index until we find it. Note that there is no
                    # call to add_token, so all of these characters will
                    # not be added as a Token since it is just comments.
                    while self.peek() != "\n" and not self.is_at_end():
                        self.advance()
                else:
                    # No second slash (/), division operator.
                    # Token(TokenType.SLASH, "/", None, 1)
                    self.add_empty_token(TokenType.SLASH)
            # Ignore whitespace.
            case " " | "\r" | "\t":
                pass
            # Newline, increment line counter and continue to scan on
            # the next line.
            case "\n":
                self.line += 1
            case '"':
                self.string()
            # Unrecognized single character, error but keep scanning,
            # just in case there are other errors we have yet to detect.
            case _:
                # Digit, begin processing number literal.
                if self.is_digit(c):
                    self.number()
                # Alphanumeric, begin parsing either an identifier of
                # either a variable name (orchid) or a reserved word (or)
                elif self.is_alpha(c):
                    self.identifier()
                # Unrecognized single character, error but keep scanning
                # just in case there are other errors we have yet to
                # detect.
                else:
                    Lox.error(self.line, "Unexpected character.")

    def identifier(self) -> None:
        """Scan and match an alphanumeric "identifier".

        An identifier can either be a reserved keyword or an identifier for
        something within a Lox source.

        Examples:
        print  -> Token(TokenType.PRINT,      "print",  None, 1)
        foobar -> Token(TokenType.IDENTIFIER, "foobar", None, 1)
        """

        # Advance current index so long as we see [a-Z_]
        while self.is_alpha_numeric(self.peek()):
            self.advance()

        # Retrieve the scanned identifier.
        text = self.source[self.start : self.current]

        # Check if identifier is a reserved word (keyword).
        # Default to user-defined identifier if unreserved.
        type = self.keywords.get(text, TokenType.IDENTIFIER)

        self.add_empty_token(type)

    def number(self) -> None:
        """Scan and match a number.

        Examples:
        4   -> TokenType(TokenType.NUMBER, "4",   4.0, 1)
        4.2 -> TokenType(TokenType.NUMBER, "4.2", 4.2, 1)
        """

        # Advance current index while continuing to scan for digits.
        while self.is_digit(self.peek()):
            self.advance()

        # Look for a fractional part and a digit after it (ie .5).
        if self.peek() == "." and self.is_digit(self.peek_next()):
            # Consume the "."
            self.advance()

            # Continue advancing current index to parse the fractional part.
            while self.is_digit(self.peek()):
                self.advance()

        # Parse the second number literal and add Token.
        # (1 is a line number)
        # 1 123.456
        # Token(TokenType.NUMBER, "123.456", 123.456, 1)
        self.add_token(TokenType.NUMBER, float(self.source[self.start : self.current]))

    def string(self) -> None:
        """Scan a Lox string.

        A scanned string in Lox is between double quotation marks, and can include
        newlines. The lexeme will contain the quotes, and the Token will include
        the Python literal representation without quotes.

        Examples:
        "foo"      -> Token(TokenType.STRING, '"foo"',      "foo",      1)
        "foo\nbar" -> Token(TokenType.STRING, '"foo\nbar"', "foo\nbar", 1)
        """

        # Continue scanning until we find the closing double quote.
        while self.peek() != '"' and not self.is_at_end():
            # Newline, increment line counter and continue scanning.
            if self.peek() == "\n":
                self.line += 1

            self.advance()

        # Scanned to the end of source without finding closing double
        # quote.
        if self.is_at_end():
            Lox.error(self.line, "Unterminated string.")
            return

        # Advance to capture the closing " that was matched in peek().
        self.advance()

        # Trim the surrounding quotes and add the scanned string Token.
        # 1 "foobar"
        # Token(TokenType.STRING, "\"foobar\"", "foobar", 1)
        value = self.source[self.start + 1 : self.current - 1]
        self.add_token(TokenType.STRING, value)

    def advance(self) -> str:
        """Retrieve the char at the Scanner's current position, then advance it.

        Returns:
            char: str. Character at Scanner's position prior to advancement.
        """

        # source.charAt(current++) increments the counter after retrieving the
        # index. Python has no postfix increment but this matches that behavior.
        current = self.source[self.current]
        self.current += 1
        return current

    def match(self, expected: str) -> bool:
        """Check for a given char at the current index and increment if found.

        If there is a match, the current index will be incremented. This is used
        for parsing multi-character lexemes such as != and ==.

        Args:
            expected. str. Expected char.

        Returns:
            matched: bool. Whether or not the char at the now previous index
                matched the expected char.
        """

        if self.is_at_end():
            # End of file, so there is no character to match.
            return False
        if self.source[self.current] != expected:
            # Current character does not match expected one
            return False

        # Current (next) character matches, increment current character
        # index so character is scanned into matching Token.
        self.current += 1
        return True

    def peek(self) -> str:
        """Return the char at the current index without consuming it.

        Returns:
            current_char: str. Char at the current index.
        """

        if self.is_at_end():
            # Return for EOF Token
            return "\0"

        return self.source[self.current]

    def peek_next(self) -> str:
        """Return the char at the current index + 1 without consuming it.

        Returns:
            next_char: str. Char at the current index + 1.
        """

        if self.current + 1 >= len(self.source):
            # Return for EOF Token
            return "\0"

        return self.source[self.current + 1]

    def is_alpha(self, c: str) -> bool:
        """Check if a given char is alphabetic or underscore [a-zA-Z_]

        Args:
            c: str. Char to check.

        Returns:
            alpha: bool. Whether char is in [a-zA-Z_]
        """

        return c.isalpha() or c == "_"

    def is_alpha_numeric(self, c: str) -> bool:
        """Check if a given char is alphanumeric (or underscore) [a-zA-Z0-9_]

        Args:
            c: str. Char to check.

        Returns:
            alpha_numeric: bool. Whether char is in [a-zA-Z0-9_]
        """

        return self.is_alpha(c) or self.is_digit(c)

    def is_digit(self, c: str) -> bool:
        """Check if a given char is a digit [0-9]

        Args:
            c: str. Char to check.

        Returns:
            digit: bool. Whether char is in [0-9]
        """

        return c.isnumeric()

    def add_empty_token(self, type: TokenType) -> None:
        """Add a Token with no eagerly parsed Python literal.

        For example Tokens of TokenType.PRINT, which are keywords, do not have
        literal values.

        Args:
            type: TokenType. Type of Token being added.
        """

        self.add_token(type, None)

    def add_token(self, type: TokenType, literal: object = None) -> None:
        """Add a Token.

        This will use the start and current indexes in the Scanner to create a
        substring which represents the lexeme for this Token.

        A literal can be passed to be stored in the Token. For example, the number
        4 would become:
        Token(type=TokenType.NUMBER, lexeme="4", literal=4.0, line=1)

        Where 4.0 is the lexeme's string representation parsed as a float. This
        is done during Scanning to avoid having to do it later.

        Args:
            type: TokenType. Type of Token being added.
            literal: object. Eagerly evaluated Python representation of the lexeme
                if any, otherwise None.
        """

        # Retrieve the Lexeme from the source using the start and current
        # values to substring the source.
        text = self.source[self.start : self.current]

        # Add the scanned Lexeme as a Token.
        self.tokens.append(Token(type, text, literal, self.line))
