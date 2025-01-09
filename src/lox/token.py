#!/usr/bin/env python3
from dataclasses import dataclass

from lox.token_type import TokenType


@dataclass(frozen=True)
class Token:
    """Scanner Token

    A Token represents a "chunk" of text within the processed source code. These
    are returned by the Scanner and consumed by the Parser.

    For example:
    var a = 2;

    Has 5 Tokens:
    Scanner("var a = 2;").scan_tokens()
    Token(TokenType.VAR,        "var", None, 1)
    Token(TokenType.IDENTIFIER, "a",   None, 1)
    Token(TokenType.EQUALS,     "=",   None, 1)
    Token(TokenType.NUMBER,     "2",   2.0,  1)
    Token(TokenType.SEMICOLON,  ";",   None, 1)

    Args:
        type: TokenType. The type of Token being identified, see the TokenType
            enum for possible types.
        lexeme: str. Scanned source contents representing this Token.
        literal: object. Eagerly evaluated Python representation of the lexeme
            if any, otherwise None.
        line: int. The line in the source code where this Token was scanned.
    """

    type: TokenType
    lexeme: str
    literal: object
    line: int

    def __repr__(self) -> str:
        return self.to_string()

    def to_string(self) -> str:
        return str(self.type) + " " + self.lexeme + " " + str(self.literal)
