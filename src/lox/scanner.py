#!/usr/bin/env python3
from typing import Dict, List

from lox.lox import Lox
from lox.token import Token
from lox.token_type import TokenType


class Scanner:
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
        while not self.is_at_end():
            self.start = self.current
            self.scan_token()

        self.tokens.append(Token(TokenType.EOF, "", None, self.line))
        return self.tokens

    def is_at_end(self) -> bool:
        return self.current >= len(self.source)

    def scan_token(self) -> None:
        c = self.advance()
        match c:
            # Single character Lexemes.
            case "(":
                self.add_empty_token(TokenType.LEFT_PAREN)
            case ")":
                self.add_empty_token(TokenType.RIGHT_PAREN)
            case "{":
                self.add_empty_token(TokenType.LEFT_BRACE)
            case "}":
                self.add_empty_token(TokenType.RIGHT_BRACE)
            case ",":
                self.add_empty_token(TokenType.COMMA)
            case ".":
                self.add_empty_token(TokenType.DOT)
            case "-":
                self.add_empty_token(TokenType.MINUS)
            case "+":
                self.add_empty_token(TokenType.PLUS)
            case ";":
                self.add_empty_token(TokenType.SEMICOLON)
            case "*":
                self.add_empty_token(TokenType.STAR)
            # Two character Lexemes.
            case "!":
                # != or !
                self.add_empty_token(
                    TokenType.BANG_EQUAL if self.match("=") else TokenType.BANG
                )
            case "=":
                # == or =
                self.add_empty_token(
                    TokenType.EQUAL_EQUAL if self.match("=") else TokenType.EQUAL
                )
            case "<":
                # <= or <
                self.add_empty_token(
                    TokenType.LESS_EQUAL if self.match("=") else TokenType.LESS
                )
            case ">":
                # >= or >
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
        # source.charAt(current++) increments the counter after retrieving the
        # index. Python has no postfix increment but this matches that behavior.
        current = self.source[self.current]
        self.current += 1
        return current

    def match(self, expected: str) -> bool:
        if self.is_at_end():
            # End of file, so there is no character to match.
            return False
        if self.source[self.current] != expected:
            # Current (next) character matches, increment current character
            # index so character is scanned into matching Token.
            return False

        # Current (next) character matches, increment current character
        # index so character is scanned into matching Token.
        self.current += 1
        return True

    def peek(self) -> str:
        # Retrieve the next char, similar to advance, without consuming it.
        if self.is_at_end():
            return "\0"

        return self.source[self.current]

    def peek_next(self) -> str:
        # Retrieve the character two ahead, similar to advance, without
        # consuming. This is as far ahead as this scanner will peek.
        if self.current + 1 >= len(self.source):
            return "\0"

        return self.source[self.current + 1]

    def is_alpha(self, c: str) -> bool:
        # Character is alphanumeric or underscore
        return c.isalpha() or c == "_"

    def is_alpha_numeric(self, c: str) -> bool:
        return self.is_alpha(c) or self.is_digit(c)

    def is_digit(self, c: str) -> bool:
        # Character is 0-9.
        return c.isnumeric()

    def add_empty_token(self, type: TokenType) -> None:
        # Add a token with no literal.
        self.add_token(type, None)

    def add_token(self, type: TokenType, literal: object) -> None:
        # Retrieve the Lexeme from the source using the start and current
        # values to substring the source.
        text = self.source[self.start : self.current]

        # Add the scanned Lexeme as a Token.
        self.tokens.append(Token(type, text, literal, self.line))
