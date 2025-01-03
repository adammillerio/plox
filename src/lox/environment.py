#!/usr/bin/env python3
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Optional

from lox.runtime_error import LoxRuntimeError
from lox.token import Token


@dataclass
class Environment:
    # Store a reference to the enclosing (parent) scope, forming a
    # parent-pointer tree or a "cactus stack", where there is a tree
    # with only one path at any given time from local to global
    enclosing: Optional[Environment] = None
    values: Dict[str, object] = field(default_factory=dict)

    def get(self, name: Token) -> object:
        # Check the local scope for this variable
        if name.lexeme in self.values:
            return self.values[name.lexeme]

        # Walk up the scope chain to see if this variable is defiend in
        # a higher level scope
        if self.enclosing is not None:
            return self.enclosing.get(name)

        # Variable is not defined in local or global scope
        raise LoxRuntimeError(name, f"Undefined variable '{name.lexeme}'.")

    def assign(self, name: Token, value: object) -> None:
        # If this variable is defined locally, assign it
        # If te same name exists locally and in enclosing scopes, the
        # local one will "shadow" any others, since this lookup always
        # happens first
        if name.lexeme in self.values:
            self.values[name.lexeme] = value
            return

        # Walk up the scope chain to see if this variable is available
        # for assignment in a higher level scope
        if self.enclosing is not None:
            self.enclosing.assign(name, value)
            return

        # Variable is not defined in local or global scope
        raise LoxRuntimeError(name, f"Undefined variable '{name.lexeme}'.")

    def define(self, name: str, value: object) -> None:
        self.values[name] = value

    def ancestor(self, distance: int) -> Environment:
        # Start at the calling environment
        environment = self

        # Walk up the environments the provided distance
        for i in range(distance):
            environment = environment.enclosing

        # Return the ancestor environment at this distance
        return environment

    def get_at(self, distance: int, name: str) -> object:
        # Resolve the ancestor environment at this distance and retrieve the
        # value for this variable name
        return self.ancestor(distance).values[name]

    def assign_at(self, distance: int, name: Token, value: object) -> None:
        # Resolve the ancestor environment at this distance and assign the
        # value to the variable declared there
        self.ancestor(distance).values[name.lexeme] = value
