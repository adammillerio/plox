#!/usr/bin/env python3
from __future__ import annotations

from typing import List, Union

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


class AstPrinter(ExprVisitor[str], StmtVisitor[str]):
    """Printer generating a human readable representation of the parsed Lox AST.

    This is a port of the full AstPrinter class in the book's sample repo:
    https://github.com/munificent/craftinginterpreters/blob/master/java/com/craftinginterpreters/lox/AstPrinter.java

    This class is partially implemented in the book, but the full version is only
    available in the sample code.

    To use the printer:
    plox print_ast examples/fibonacciRecursive.lox
    1: (fun fib(n) (if (<= n 1.0) (return n))(return (+ (call fib  (- n 2.0)) (call fib  (- n 1.0)))))
    2: (block (var i = 0.0)(while (< i 20.0) (block (block (print (call fib  i)))(; (= i (+ i 1.0))))))
    """

    def print(self, expr_or_stmt: Union[Expr, Stmt]) -> str:
        return expr_or_stmt.accept(self)

    def visit_block_stmt(self, stmt: Block) -> str:
        builder = ""
        builder += "(block "

        for statement in stmt.statements:
            builder += self.print(statement)

        builder += ")"
        return builder

    def visit_class_stmt(self, stmt: Class) -> str:
        builder = ""
        builder += "(class "

        if stmt.superclass is not None:
            builder += f" < {self.print(stmt.superclass)}"

        for method in stmt.methods:
            builder += f" {self.print(method)}"

        builder += ")"
        return builder

    def visit_expression_stmt(self, stmt: Expression) -> str:
        return self.parenthesize(";", stmt.expression)

    def visit_function_stmt(self, stmt: Function) -> str:
        builder = ""
        builder += f"(fun {stmt.name.lexeme}("

        for i, param in enumerate(stmt.params):
            if i != 0:
                builder += " "

            builder += param.lexeme

        builder += ") "

        for body in stmt.body:
            builder += self.print(body)

        builder += ")"
        return builder

    def visit_if_stmt(self, stmt: If) -> str:
        if stmt.else_branch is None:
            return self.parenthesize2("if", stmt.condition, stmt.then_branch)

        return self.parenthesize2(
            "if-else", stmt.condition, stmt.then_branch, stmt.else_branch
        )

    def visit_print_stmt(self, stmt: Print) -> str:
        return self.parenthesize("print", stmt.expression)

    def visit_return_stmt(self, stmt: Return) -> str:
        if stmt.value is None:
            return "(return)"

        return self.parenthesize("return", stmt.value)

    def visit_var_stmt(self, stmt: Var) -> str:
        if stmt.initializer is None:
            return self.parenthesize2("var", stmt.name)

        return self.parenthesize2("var", stmt.name, "=", stmt.initializer)

    def visit_while_stmt(self, stmt: While) -> str:
        return self.parenthesize2("while", stmt.condition, stmt.body)

    def visit_assign_expr(self, expr: Assign) -> str:
        return self.parenthesize2("=", expr.name.lexeme, expr.value)

    def visit_binary_expr(self, expr: Binary) -> str:
        return self.parenthesize(expr.operator.lexeme, expr.left, expr.right)

    def visit_call_expr(self, expr: Call) -> str:
        return self.parenthesize2("call", expr.callee, expr.arguments)

    def visit_get_expr(self, expr: Get) -> str:
        return self.parenthesize2(".", expr.object, expr.name.lexeme)

    def visit_grouping_expr(self, expr: Grouping) -> str:
        return self.parenthesize("group", expr.expression)

    def visit_literal_expr(self, expr: Literal) -> str:
        if expr.value is None:
            return "nil"

        return str(expr.value)

    def visit_logical_expr(self, expr: Logical) -> str:
        return self.parenthesize(expr.operator.lexeme, expr.left, expr.right)

    def visit_set_expr(self, expr: Set) -> str:
        return self.parenthesize2("=", expr.object, expr.name.lexeme, expr.value)

    def visit_super_expr(self, expr: Super) -> str:
        return self.parenthesize2("super", expr.method)

    def visit_this_expr(self, expr: This) -> str:
        return "this"

    def visit_unary_expr(self, expr: Unary) -> str:
        return self.parenthesize(expr.operator.lexeme, expr.right)

    def visit_variable_expr(self, expr: Variable) -> str:
        return expr.name.lexeme

    def parenthesize(self, name: str, *exprs: Expr) -> str:
        builder = ""

        builder += f"({name}"
        for expr in exprs:
            builder += f" {self.print(expr)}"
        builder += ")"

        return builder

    def parenthesize2(self, name: str, *parts: object) -> str:
        builder = ""

        builder += f"({name}"
        builder = self.transform(builder, *parts)
        builder += ")"

        return builder

    def transform(self, builder: str, *parts: object) -> str:
        for part in parts:
            builder += " "
            if isinstance(part, Expr):
                builder += self.print(part)
            elif isinstance(part, Stmt):
                builder += self.print(part)
            elif isinstance(part, Token):
                builder += part.lexeme
            elif isinstance(part, List):
                builder = self.transform(builder, *part)
            else:
                builder += str(part)

        return builder
