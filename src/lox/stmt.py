#!/usr/bin/env python3
from __future__ import annotations

from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Generic, List, Optional, TypeVar

from lox.expr import Expr, Variable
from lox.token import Token

R = TypeVar("R", covariant=True)


@dataclass(eq=True, frozen=True)
class Stmt(metaclass=ABCMeta):
    @abstractmethod
    def accept(self, visitor: Visitor[R]) -> R: ...


@dataclass(eq=True, frozen=True)
class Block(Stmt):
    statements: List[Stmt]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_block_stmt(self)


@dataclass(eq=True, frozen=True)
class Class(Stmt):
    name: Token
    superclass: Optional[Variable]
    methods: List[Function]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_class_stmt(self)


@dataclass(eq=True, frozen=True)
class Expression(Stmt):
    expression: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_expression_stmt(self)


@dataclass(eq=True, frozen=True)
class Function(Stmt):
    name: Token
    params: List[Token]
    body: List[Stmt]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_function_stmt(self)


@dataclass(eq=True, frozen=True)
class If(Stmt):
    condition: Expr
    then_branch: Stmt
    else_branch: Optional[Stmt] = None

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_if_stmt(self)


@dataclass(eq=True, frozen=True)
class Print(Stmt):
    expression: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_print_stmt(self)


@dataclass(eq=True, frozen=True)
class Return(Stmt):
    keyword: Token
    value: Optional[Expr] = None

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_return_stmt(self)


@dataclass(eq=True, frozen=True)
class Var(Stmt):
    name: Token
    initializer: Optional[Expr] = None

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_var_stmt(self)


@dataclass(eq=True, frozen=True)
class While(Stmt):
    condition: Expr
    body: Stmt

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_while_stmt(self)


class Visitor(Generic[R], metaclass=ABCMeta):
    @abstractmethod
    def visit_block_stmt(self, stmt: Block) -> R: ...

    @abstractmethod
    def visit_class_stmt(self, stmt: Class) -> R: ...

    @abstractmethod
    def visit_expression_stmt(self, stmt: Expression) -> R: ...

    @abstractmethod
    def visit_function_stmt(self, stmt: Function) -> R: ...

    @abstractmethod
    def visit_if_stmt(self, stmt: If) -> R: ...

    @abstractmethod
    def visit_print_stmt(self, stmt: Print) -> R: ...

    @abstractmethod
    def visit_return_stmt(self, stmt: Return) -> R: ...

    @abstractmethod
    def visit_var_stmt(self, stmt: Var) -> R: ...

    @abstractmethod
    def visit_while_stmt(self, stmt: While) -> R: ...
