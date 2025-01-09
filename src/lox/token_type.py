#!/usr/bin/env python3
from enum import IntEnum, auto


class TokenType(IntEnum):
    """Token types

    Each TokenType represents a distinctly identifiable piece of text within the
    source code. These are assigned by the Scanner class as it processes the
    source in order to provide a consistently identifiable set of Tokens for
    the Parser.
    """

    # Single-character tokens.
    LEFT_PAREN = auto()  # (
    RIGHT_PAREN = auto()  # )
    LEFT_BRACE = auto()  # {
    RIGHT_BRACE = auto()  # }
    COMMA = auto()  # ,
    DOT = auto()  # .
    MINUS = auto()  # -
    PLUS = auto()  # +
    SEMICOLON = auto()  # ;
    SLASH = auto()  # /
    STAR = auto()  # *

    # One or two character tokens
    BANG = auto()  # !
    BANG_EQUAL = auto()  # !=
    EQUAL = auto()  # =
    EQUAL_EQUAL = auto()  # ==
    GREATER = auto()  # >
    GREATER_EQUAL = auto()  # >=
    LESS = auto()  # <
    LESS_EQUAL = auto()  # <=

    # Literals
    IDENTIFIER = auto()  # fooVar
    STRING = auto()  # "foobar"
    NUMBER = auto()  # 42

    # Keywords
    # These words are identifiers reserved for use within the Lox language itself
    AND = auto()
    CLASS = auto()
    ELSE = auto()
    FALSE = auto()
    FUN = auto()
    FOR = auto()
    IF = auto()
    NIL = auto()
    OR = auto()
    PRINT = auto()
    RETURN = auto()
    SUPER = auto()
    THIS = auto()
    TRUE = auto()
    VAR = auto()
    WHILE = auto()

    # End of file
    EOF = auto()  # \0
