#!/usr/bin/env python3
from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Dict, List, Optional

from lox.lox_callable import LoxCallable
from lox.lox_function import LoxFunction
from lox.lox_instance import LoxInstance

if TYPE_CHECKING:
    from lox.interpreter import Interpreter


@dataclass
class LoxClass(LoxCallable):
    name: str
    superclass: Optional[LoxClass] = None
    methods: Dict[str, LoxFunction] = field(default_factory=dict)

    def __repr__(self) -> str:
        return self.to_string()

    def find_method(self, name: str) -> Optional[LoxFunction]:
        if name in self.methods:
            return self.methods[name]

        # Check if the method exists on the superclass if one is defined
        if self.superclass is not None:
            return self.superclass.find_method(name)

        return None

    def to_string(self) -> str:
        return self.name

    def call(self, interpreter: "Interpreter", arguments: List[object]) -> object:
        instance = LoxInstance(self)

        # Look up the constructor method if any, bind it, and execute it
        initializer = self.find_method("init")
        if initializer is not None:
            initializer.bind(instance).call(interpreter, arguments)

        return instance

    def arity(self) -> int:
        initializer = self.find_method("init")
        # No constructor method
        if initializer is None:
            return 0

        # Return the arity of the constructor method
        return initializer.arity()
