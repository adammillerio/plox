#!/usr/bin/env python3
from collections import deque
from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import Dict, List

from lox.expr import (
    Assign,
    Binary,
    Call,
    Expr,
    Get,
    Grouping,
    Literal,
    Logical,
    Set,
    Super,
    This,
    Unary,
    Variable,
)
from lox.expr import Visitor as ExprVisitor
from lox.interpreter import Interpreter
from lox.lox import Lox
from lox.stmt import (
    Block,
    Class,
    Expression,
    Function,
    If,
    Print,
    Return,
    Stmt,
    Var,
    While,
)
from lox.stmt import Visitor as StmtVisitor
from lox.token import Token


class FunctionType(IntEnum):
    # Top-level code
    NONE = auto()
    # Function definition
    FUNCTION = auto()
    # Initializer (constructor method on a class)
    INITIALIZER = auto()
    # Method, which is a function defined on a class and bound to it's
    # instances
    METHOD = auto()


class ClassType(IntEnum):
    # Top-level code
    NONE = auto()
    # Class definition
    CLASS = auto()
    # Subclass definition
    SUBCLASS = auto()


@dataclass
class Resolver(ExprVisitor[None], StmtVisitor[None]):
    interpreter: Interpreter
    scopes: deque[Dict[str, bool]] = field(default_factory=deque)
    current_function: FunctionType = FunctionType.NONE
    current_class: ClassType = ClassType.NONE

    def resolve(self, statements: List[Stmt]) -> None:
        for statement in statements:
            self.resolve_stmt(statement)

    def resolve_stmt(self, stmt: Stmt) -> None:
        stmt.accept(self)

    def resolve_expr(self, expr: Expr) -> None:
        expr.accept(self)

    def resolve_function(self, function: Function, type: FunctionType) -> None:
        # Store the previous value, since functions can be defined in functions
        enclosing_function = self.current_function
        # Set the new current function type
        self.current_function = type

        # Create a new local scope for the function call
        self.begin_scope()

        for param in function.params:
            # Declare and define all function parameters, so that they are
            # available during function body resolution
            self.declare(param)
            self.define(param)

        self.resolve(function.body)
        self.end_scope()

        # Restore previous function type
        self.current_function = enclosing_function

    def begin_scope(self) -> None:
        self.scopes.append({})

    def end_scope(self) -> None:
        self.scopes.pop()

    def declare(self, name: Token) -> None:
        if not self.scopes:
            return

        scope = self.scopes[-1]

        # Error on variable redefinition outside of the global scope
        # var a = "first"
        # var a = "second"; <- Error
        if name.lexeme in scope:
            Lox.error(name, "Already a variable with this scope.")

        # Place the var in the scope on tbe top of the stack, with a false to
        # indicate that it is not initialized yet. This prevents it from being
        # referenced prior to initialization.
        # var a = "outer";
        # { var a = a; } <- Error, a is in scope but still false
        scope[name.lexeme] = False

    def define(self, name: Token) -> None:
        if not self.scopes:
            return

        # Mark the var as initialized, so that it can be used
        self.scopes[-1][name.lexeme] = True

    def resolve_local(self, expr: Expr, name: Token) -> None:
        # Traverse backwards through the scope stack
        for i in range(len(self.scopes) - 1, -1, -1):
            # This scope has a variable with this name
            if name.lexeme in self.scopes[i]:
                # Resolve the variable corresponding to this expression with
                # the number of scopes backward that it is located
                self.interpreter.resolve(expr, len(self.scopes) - 1 - i)
                return

    def visit_block_stmt(self, stmt: Block) -> None:
        self.begin_scope()
        self.resolve(stmt.statements)
        self.end_scope()

    def visit_class_stmt(self, stmt: Class) -> None:
        # Store the previous value, since classes can be defined in classes
        enclosing_class = self.current_class
        # Set the new current class type
        self.current_class = ClassType.CLASS

        self.declare(stmt.name)
        self.define(stmt.name)

        # class Foo < Foo {} <- Error
        if (
            stmt.superclass is not None
            and stmt.name.lexeme == stmt.superclass.name.lexeme
        ):
            Lox.error(stmt.superclass.name, "A class can't inherit from itself.")

        # Resolve the superclass variable if there is one
        if stmt.superclass is not None:
            # Now resolving a subclass
            self.current_class = ClassType.SUBCLASS
            self.resolve_expr(stmt.superclass)

        # Create a new scope and place the "super" variable in it, so that
        # it will be available in the subclass
        if stmt.superclass is not None:
            self.begin_scope()
            self.scopes[-1]["super"] = True

        # Create a new enclosing scope for this method definition
        self.begin_scope()
        # Register "this" as a variable, to be used in class methods
        self.scopes[-1]["this"] = True

        for method in stmt.methods:
            declaration = FunctionType.METHOD
            self.resolve_function(method, declaration)

        # Discard the scope with "this"
        self.end_scope()

        # Discard the scope with "super" if there is a superclass
        if stmt.superclass is not None:
            self.end_scope()

        # Restore previous value
        self.current_class = enclosing_class

    def visit_expression_stmt(self, stmt: Expression) -> None:
        # Resolve a single expression statement
        self.resolve_expr(stmt.expression)

    def visit_function_stmt(self, stmt: Function) -> None:
        # Declare and define the function itself in the enclosing scope, this
        # is done eagerly before resolving the body, in order to allow the
        # function body to refer to itself during recursion
        self.declare(stmt.name)
        self.define(stmt.name)

        self.resolve_function(stmt, FunctionType.FUNCTION)

    def visit_if_stmt(self, stmt: If) -> None:
        # Resolve if condition and then branch, there is no control flow here,
        # so all paths are traversed during resolution
        self.resolve_expr(stmt.condition)
        self.resolve_stmt(stmt.then_branch)

        if stmt.else_branch is not None:
            # Resolve else branch if it exists
            self.resolve_stmt(stmt.else_branch)

    def visit_print_stmt(self, stmt: Print) -> None:
        # Resolve the expression which is being printed
        self.resolve_expr(stmt.expression)

    def visit_return_stmt(self, stmt: Return) -> None:
        # Not in any function, can't return
        if self.current_function is FunctionType.NONE:
            Lox.error(stmt.keyword, "Can't return from top-level code.")

        # Resolve expression being returned, if present
        if stmt.value is not None:
            # Constructor/initializer methods always return this implicitly,
            # so any return with a value in one is an error
            if self.current_function is FunctionType.INITIALIZER:
                Lox.error(stmt.keyword, "Can't return a value from an initializer.")

            # Redundant check for type checker
            assert stmt.value is not None
            self.resolve_expr(stmt.value)

    def visit_var_stmt(self, stmt: Var) -> None:
        self.declare(stmt.name)

        if stmt.initializer is not None:
            self.resolve_expr(stmt.initializer)

        self.define(stmt.name)

    def visit_while_stmt(self, stmt: While) -> None:
        # Resolve the condition and body, no control flow, so the loop is
        # always evaluated once
        self.resolve_expr(stmt.condition)
        self.resolve_stmt(stmt.body)

    def visit_assign_expr(self, expr: Assign) -> None:
        # Resolve the expression to be assigned for references to other vars
        # ie var a = i + 1
        self.resolve_expr(expr.value)
        # Resolve the variable that is being assigned to
        self.resolve_local(expr, expr.name)

    def visit_binary_expr(self, expr: Binary) -> None:
        # Resolve both operands of the binary expression
        self.resolve_expr(expr.left)
        self.resolve_expr(expr.right)

    def visit_call_expr(self, expr: Call) -> None:
        # Resolve the callee itself
        self.resolve_expr(expr.callee)

        for argument in expr.arguments:
            # Resolve all arguments to the call
            self.resolve_expr(argument)

    def visit_get_expr(self, expr: Get) -> None:
        # Resolve the left side of a property access/get expression
        # The right side (property dispatch) happens at runtime in the
        # interpreter
        self.resolve_expr(expr.object)

    def visit_grouping_expr(self, expr: Grouping) -> None:
        # Resolve expression inside the parenthesis
        self.resolve_expr(expr.expression)

    def visit_literal_expr(self, expr: Literal) -> None:
        # This is a literal, no resolution necessary
        pass

    def visit_logical_expr(self, expr: Logical) -> None:
        # Resolve both sides of the logical expression, with no short circuit
        self.resolve_expr(expr.left)
        self.resolve_expr(expr.right)

    def visit_set_expr(self, expr: Set) -> None:
        self.resolve_expr(expr.value)
        self.resolve_expr(expr.object)

    def visit_super_expr(self, expr: Super) -> None:
        if self.current_class is ClassType.NONE:
            # super.NotInClass() <- Error
            Lox.error(expr.keyword, "Can't use 'super' outside of a class.")
        elif self.current_class is not ClassType.SUBCLASS:
            # class Foo { method() { print super.method() } } <- Error
            Lox.error(expr.keyword, "Can't use 'super' in a class with no superclass.")

        self.resolve_local(expr, expr.keyword)

    def visit_this_expr(self, expr: This) -> None:
        if self.current_class is ClassType.NONE:
            # this keyword used outside of class scope, error
            Lox.error(expr.keyword,
                     "Can't use 'this' outside of a class.")
            return

        self.resolve_local(expr, expr.keyword)

    def visit_unary_expr(self, expr: Unary) -> None:
        # Resolve the unary operand
        self.resolve_expr(expr.right)

    def visit_variable_expr(self, expr: Variable) -> None:
        # Variable is declared but not initialized yet
        # Ruff doesn't like the direct comparison to False but get returns None
        # when unset, which is also falsey
        if self.scopes and self.scopes[-1].get(expr.name.lexeme) == False: # noqa: E712
            Lox.error(expr.name,
                      "Can't read local variable in its own initializer.")

        self.resolve_local(expr, expr.name)
