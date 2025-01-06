#!/usr/bin/env python3
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Dict

from lox.runtime_error import LoxRuntimeError
from lox.token import Token

if TYPE_CHECKING:
    from lox.lox_class import LoxClass


@dataclass
class LoxInstance:
    klass: "LoxClass"
    fields: Dict[str, object] = field(default_factory=dict)

    def __repr__(self) -> str:
        return self.to_string()

    def get(self, name: Token) -> object:
        if name.lexeme in self.fields:
            return self.fields[name.lexeme]

        method = self.klass.find_method(name.lexeme)
        if method is not None:
            return method.bind(self)

        raise LoxRuntimeError(name, f"Undefined property '{name.lexeme}'.")

    def set(self, name: Token, value: object) -> None:
        self.fields[name.lexeme] = value

    def to_string(self) -> str:
        return f"{self.klass.name} instance"
