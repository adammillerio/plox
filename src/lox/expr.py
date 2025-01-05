#!/usr/bin/env python3
from __future__ import annotations

from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Generic, List, TypeVar

from lox.token import Token

R = TypeVar("R", covariant=True)


@dataclass(eq=False, frozen=True)
class Expr(metaclass=ABCMeta):
    @abstractmethod
    def accept(self, visitor: Visitor[R]) -> R: ...


@dataclass(eq=False, frozen=True)
class Assign(Expr):
    name: Token
    value: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_assign_expr(self)


@dataclass(eq=False, frozen=True)
class Binary(Expr):
    left: Expr
    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_binary_expr(self)


@dataclass(eq=False, frozen=True)
class Call(Expr):
    callee: Expr
    paren: Token
    arguments: List[Expr]

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_call_expr(self)


@dataclass(eq=False, frozen=True)
class Get(Expr):
    object: Expr
    name: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_get_expr(self)


@dataclass(eq=False, frozen=True)
class Grouping(Expr):
    expression: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_grouping_expr(self)


@dataclass(eq=False, frozen=True)
class Literal(Expr):
    value: object

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_literal_expr(self)


@dataclass(eq=False, frozen=True)
class Logical(Expr):
    left: Expr
    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_logical_expr(self)


@dataclass(eq=False, frozen=True)
class Set(Expr):
    object: Expr
    name: Token
    value: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_set_expr(self)


@dataclass(eq=False, frozen=True)
class Super(Expr):
    keyword: Token
    method: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_super_expr(self)


@dataclass(eq=False, frozen=True)
class This(Expr):
    keyword: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_this_expr(self)


@dataclass(eq=False, frozen=True)
class Unary(Expr):
    operator: Token
    right: Expr

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_unary_expr(self)


@dataclass(eq=False, frozen=True)
class Variable(Expr):
    name: Token

    def accept(self, visitor: Visitor[R]) -> R:
        return visitor.visit_variable_expr(self)


class Visitor(Generic[R], metaclass=ABCMeta):
    @abstractmethod
    def visit_assign_expr(self, expr: Assign) -> R: ...

    @abstractmethod
    def visit_binary_expr(self, expr: Binary) -> R: ...

    @abstractmethod
    def visit_call_expr(self, expr: Call) -> R: ...

    @abstractmethod
    def visit_get_expr(self, expr: Get) -> R: ...

    @abstractmethod
    def visit_grouping_expr(self, expr: Grouping) -> R: ...

    @abstractmethod
    def visit_literal_expr(self, expr: Literal) -> R: ...

    @abstractmethod
    def visit_logical_expr(self, expr: Logical) -> R: ...

    @abstractmethod
    def visit_set_expr(self, expr: Set) -> R: ...

    @abstractmethod
    def visit_super_expr(self, expr: Super) -> R: ...

    @abstractmethod
    def visit_this_expr(self, expr: This) -> R: ...

    @abstractmethod
    def visit_unary_expr(self, expr: Unary) -> R: ...

    @abstractmethod
    def visit_variable_expr(self, expr: Variable) -> R: ...
