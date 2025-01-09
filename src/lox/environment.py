#!/usr/bin/env python3
from __future__ import annotations

from typing import Dict, Optional

from lox.runtime_error import LoxRuntimeError
from lox.token import Token


class Environment:
    """Lox Environment

    An Environment holds all of the values defined in a given scope.

    Scoping in Lox is implemented as a parent-pointer tree, or "cactus stack",
    where there is a tree of enclosing Environments with only one path at any
    given time from local to global. That is, while there are many Environments
    created, linked, and deleted as the interpreter enters and exits scopes, the
    path backwards to the parent is always linear.

    For more info, see the side note in Chapter 8 "Nesting and Shadowing":
    https://craftinginterpreters.com/statements-and-state.html#nesting-and-shadowing

    Before executing any parsed statements, there is a Resolver process, which
    walks the AST to track this depth mapping for efficient resolution. See
    the Resolver class for more info.

    Args:
        enclosing: Optional[Environment]. Reference to the enclosing (parent) scope.
            If none is set, this is the global scope.

    Public Attributes:
        values: Dict[str, object]. Mapping of variable names to their runtime
            Python values for this scope.
    """

    def __init__(self, enclosing: Optional[Environment] = None) -> None:
        self.enclosing = enclosing
        self.values: Dict[str, object] = {}

    def get(self, name: Token) -> object:
        """Retrieve the value for a given variable.

        Given a name, this will check the local scope, and walk up until it finds
        an Environment with this name in it's values, returning the runtime
        value stored for it.

        Args:
            name: Token. Token representing the name to retrieve.

        Returns:
            value: object. Retrieved value.

        Raises:
            LoxRuntimeError: If a variable with this name is not defined in any
                local scope or the global scope.
        """

        # Check the local scope for this variable
        if name.lexeme in self.values:
            return self.values[name.lexeme]

        # Walk up the scope chain to see if this variable is defined in
        # a higher level scope
        if self.enclosing is not None:
            return self.enclosing.get(name)

        # Variable is not defined in local or global scope
        raise LoxRuntimeError(name, f"Undefined variable '{name.lexeme}'.")

    def assign(self, name: Token, value: object) -> None:
        """Assign a value to a defined variable.

        Given a name, this will check the local scope, and walk up until it finds
        an Environment with this name in it's values. If one is defined, it will
        assign the provided value.

        Args:
            name: Token. Token representing the name to retrieve.
            value: object. Value to set for this variable.

        Raises:
            LoxRuntimeError: If a variable with this name is not defined in any
                local scope or the global scope.
        """

        # If this variable is defined locally, assign it
        # If the same name exists locally and in enclosing scopes, the
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
        """Define a variable.

        This defines a variable in this Environment's values.

        Args:
            name: str. Name of the variable being defined.
            value: object. Value to set for this variable.
        """

        self.values[name] = value

    def ancestor(self, distance: int) -> Environment:
        """Retrieve an Environment at a given distance up the scope chain.

        This is used by get_at/assign_at via the Interpreter's look_up_variable
        method to quickly resolve the location of a given variable.

        Args:
            distance: int. Distance up the scope chain to walk.

        Returns:
            environment: Environment. The Environment at this distance in the
                scope chain.
        """

        # Start at the calling environment
        environment = self

        # Walk up the environments the provided distance
        for i in range(distance):
            environment = environment.enclosing

        # Return the ancestor environment at this distance
        return environment

    def get_at(self, distance: int, name: str) -> object:
        """Get a variable at a given distance in the scope tree.

        This resolves the ancestor environment at this distance and retrieves the
        value for this variable name.

        Args:
            distance: int. Distance up the scope chain to walk.
            name: str. Name of the variable to retrieve.

        Returns:
            value: object. Retrieved value at this distance.
        """

        # Resolve the ancestor environment at this distance and retrieve the
        # value for this variable name
        return self.ancestor(distance).values[name]

    def assign_at(self, distance: int, name: Token, value: object) -> None:
        """Assign a value to a variable at a given distance in the scope tree.

        This resolves the ancestor environment at this distance and assigns the
        value to the variable declared there.

        Args:
            distance: int. Distance up the scope chain to walk.
            name: str. Name of the variable being assigned.
            value: object. Value to set for this variable.
        """

        # Resolve the ancestor environment at this distance and assign the
        # value to the variable declared there
        self.ancestor(distance).values[name.lexeme] = value
