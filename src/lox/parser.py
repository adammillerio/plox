#!/usr/bin/env python3
from typing import List, Optional, Union

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
from lox.lox import Lox
from lox.runtime_error import LoxRuntimeError
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
from lox.token import Token
from lox.token_type import TokenType

# Type aliases to common collections of exprs/stmts for return value refinement
ParsedPrimary = Union[Literal, Super, This, Variable, Grouping]
ParsedCall = Union[ParsedPrimary, Call, Get]
ParsedUnary = Union[Unary, ParsedCall]
ParsedEquality = Union[Binary, ParsedUnary]
ParsedLogicOr = Union[Logical, ParsedEquality]


class ParseError(LoxRuntimeError):
    """Runtime error for any fatal errors encountered during parsing."""

    pass


class Parser:
    """Lox Parser

    This class processes a set of Tokens provided by the Scanner and generates
    Lox statements, to be used by the Resolver and Interpreter.

    Args:
        tokens: List[Token]. Tokens to process, typically generated from the Scanner
            against a source text.

    Public Attributes:
        current: int. Current token index of the parser.
    """

    def __init__(self, tokens: List[Token]) -> None:
        self.tokens = tokens
        self.current = 0

    def parse(self) -> List[Stmt]:
        """Parse all tokens and return the expression.

        This is the top level of the parser, returning all statements parsed
        from the Tokens provided.

        Grammar Rule:
        program → declaration* EOF ;

        Returns:
            stmts: List[Stmt]. Parsed statements.
        """

        statements = []

        while not self.is_at_end():
            statements.append(self.declaration())

        return statements

    def expression(self) -> Expr:
        """Parse an expression.

        Grammar Rule:
        expression → assignment ;

        Returns:
            expression: Expr. Parsed expression.
        """

        return self.assignment()

    # declaration → classDecl | funDecl | varDecl | statement ;
    def declaration(self) -> Optional[Stmt]:
        """Parse a declaration statement.

        If a parser error is encountered, this will synchronize to the next valid
        statement and return None.

        Grammar Rule:
        declaration → classDecl | funDecl | varDecl | statement ;

        Returns:
            statement: Optional[Stmt]. The parsed declaration statement, if
                successful.
        """

        try:
            if self.match(TokenType.CLASS):
                return self.class_declaration()

            if self.match(TokenType.FUN):
                return self.function("function")

            if self.match(TokenType.VAR):
                return self.var_declaration()

            return self.statement()
        except ParseError:
            self.synchronize()
            return None

    def class_declaration(self) -> Class:
        """Parse a class declaration.

        Grammar Rule:
        classDecl → "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;

        Return:
            class: Class. Parsed Class declaration statement.
        """

        # class Foo < Bar { add(a, b, c) { print a + b + c; } }
        # name = "Foo"
        name = self.consume(TokenType.IDENTIFIER, "Expect class name.")

        # Get superclass if any
        # superclass = Bar
        superclass = None
        if self.match(TokenType.LESS):
            self.consume(TokenType.IDENTIFIER, "Expect superclass name.")
            superclass = Variable(self.previous())

        self.consume(TokenType.LEFT_BRACE, "Expect '{' before class body.")

        # Add all methods using function parser
        methods = []
        while not self.check(TokenType.RIGHT_BRACE) and not self.is_at_end():
            methods.append(self.function("method"))

        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.")

        return Class(name, superclass, methods)

    def statement(self) -> Union[While, If, Print, Return, Block, Expression]:
        """Parse a non-declarative statement.

        These statements do not declare/bind anything.

        Grammar Rule:
        statement → exprStatement | forStmt | ifStmt | printStmt | returnStmt
                  | whileStmt | block ;

        Returns:
            statement: Stmt. Parsed non-declarative statement.
        """

        if self.match(TokenType.FOR):
            return self.for_statement()
        if self.match(TokenType.IF):
            return self.if_statement()
        if self.match(TokenType.PRINT):
            return self.print_statement()
        if self.match(TokenType.RETURN):
            return self.return_statement()
        if self.match(TokenType.WHILE):
            return self.while_statement()
        if self.match(TokenType.LEFT_BRACE):
            return Block(self.block())

        return self.expression_statement()

    def for_statement(self) -> Union[Block, While]:
        """Parse a for-loop statement.

        In Lox, for loops are desugared into an equivalent representation based
        on Block, Var, and While statements. See the docs for lox.stmt.While for
        more info.

        Grammar Rule:
        forStmt → "for" "(" ( varDecl | exprStmt | ";" )
                            expression? ";"
                            expression? ")" statement ;

        Returns:
            statement: Union[Block, While]. For loop parsed and desugared into
                       Var/While statements.
        """

        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")

        if self.match(TokenType.SEMICOLON):
            # ; (no initializer)
            initializer = None
        elif self.match(TokenType.VAR):
            # var i = 0;
            initializer = self.var_declaration()
        else:
            # i = 0;
            initializer = self.expression_statement()

        # Parse the condition
        # No condition by default if there is just a semicolon
        condition = None
        if not self.check(TokenType.SEMICOLON):
            # i < 10
            condition = self.expression()
        self.consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")

        # Parse the increment
        # No increment by default if there is just the closing right paren
        increment = None
        if not self.check(TokenType.RIGHT_PAREN):
            # i = i + 1
            increment = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")
        # Parse the statement body
        # print i
        body = self.statement()

        # Desugar the for loop, breaking it down into existing statements
        # and expressions, rather than defining a new node in the AST
        # for (var i = 0; i < 10; i = i + 1) print i;
        # becomes
        # { var i = 0; while (i < 10) { print i; i = i + 1;} }
        # Desugar the increment
        if increment is not None:
            # Enclose the body statement with a block which contains it
            # along with an expression which evaluates the increment
            body = Block([body, Expression(increment)])

        # Desugar the condition
        # Default to infinite loop if no condition (while true)
        if condition is None:
            condition = Literal(True)

        # Enclose the body statement in a While statement which evaluates
        # the condition each iteration
        body = While(condition, body)

        # Desugar the initializer
        if initializer is not None:
            # Enclose the body in a block which evaluates the initializer
            # before running it
            body = Block([initializer, body])

        return body

    def if_statement(self) -> If:
        """Parse an if statement.

        Grammar Rule:
        ifStmt  → "if" "(" expression ")" statement
                  ( "else" statement )? ;

        Returns:
            statement: If. Parsed if statement.
        """

        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
        condition = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")

        then_branch = self.statement()
        else_branch = None
        if self.match(TokenType.ELSE):
            else_branch = self.statement()

        return If(condition, then_branch, else_branch)

    def var_declaration(self) -> Var:
        """Parse a variable declaration.

        Variable declarations declare and optionally define a variable.

        Grammar Rule:
        varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;

        Returns:
            declaration: Var. Parsed variable declaration.
        """

        name = self.consume(TokenType.IDENTIFIER, "Expect variable name.")

        initializer = None
        if self.match(TokenType.EQUAL):
            initializer = self.expression()

        self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")

        return Var(name, initializer)

    def while_statement(self) -> While:
        """Parse a while statement.

        Grammar Rule:
        whileStmt → "while" "(" expression ")" statement ;

        Returns:
            statement: While. Parsed while statement.
        """

        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
        condition = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")

        body = self.statement()

        return While(condition, body)

    def print_statement(self) -> Print:
        """Parse a print statement.

        Grammar Rule:
        printStmt → "print" expression ";" ;

        Returns:
            statement: Print. Parsed print statement.
        """

        value = self.expression()

        self.consume(TokenType.SEMICOLON, "Expect ';' after value.")

        return Print(value)

    def return_statement(self) -> Return:
        """Parse a return statement.

        Grammar Rule:
        returnStmt → "return" expression? ";" ;

        Returns:
            statement: Return. Parsed return statement.
        """

        keyword = self.previous()

        # return;
        value = None
        if not self.check(TokenType.SEMICOLON):
            # If the next token is not the semicolon, parse the expression
            # being returned ie:
            # return 1;
            value = self.expression()

        self.consume(TokenType.SEMICOLON, "Expect ';' after expression.")
        return Return(keyword, value)

    def expression_statement(self) -> Expression:
        """Parse an expression statement.

        Since the interpreter operates on a list of statements, this is a simple
        statement which encapsulates a bare expression.

        Grammar Rule:
        exprStmt → expression ";" ;

        Returns:
            statement: Expression. Parsed expression statement.
        """

        expr = self.expression()

        self.consume(TokenType.SEMICOLON, "Expect ';' after expression.")

        return Expression(expr)

    def function(self, kind: str) -> Function:
        """Parse a function statement.

        Grammar Rules:
        function   → IDENTIFIER "(" parameters? ")" block ;
        parameters → IDENTIFIER ( "," IDENTIFIER )* ;

        Args:
            kind: str. Kind of function, either a class method or a function.

        Returns:
            statement: Function. Parsed function statement.
        """

        # fun add(a, b, c) { print a + b + c; }
        # Parse the name of the function
        # name = add
        name = self.consume(TokenType.IDENTIFIER, f"Expect {kind} name.")

        self.consume(TokenType.LEFT_PAREN, f"Expect '(' after {kind} name.")

        # Parse all function parameters, up to 255
        # parameters = ["a", "b", "c"]
        # This includes the zero-parameter case
        parameters = []
        if not self.check(TokenType.RIGHT_PAREN):
            parameters.append(
                self.consume(TokenType.IDENTIFIER, "Expect parameter name.")
            )
            while self.match(TokenType.COMMA):
                if len(parameters) >= 255:
                    self.error(self.peek(), "Can't have more than 255 parameters.")

                parameters.append(
                    self.consume(TokenType.IDENTIFIER, "Expect parameter name.")
                )

        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.")

        # Parse the function body block
        # body = [<print statement>]
        self.consume(TokenType.LEFT_BRACE, f"Expect '{{' before {kind} body.")
        body = self.block()

        return Function(name, parameters, body)

    def block(self) -> List[Stmt]:
        """Parse a block of statements.

        This returns the list of statements which are wrapped in a Block statement.

        Grammar Rule:
        block → "{" declaration* "}" ;

        Returns:
            statements: List[Stmt]. List of all parsed statements.
        """

        statements = []

        while not self.check(TokenType.RIGHT_BRACE) and not self.is_at_end():
            statements.append(self.declaration())

        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
        return statements

    # assignment → ( call "." )? IDENTIFIER "=" assignment
    # | logic_or ;
    def assignment(self) -> Union[Assign, Set, ParsedLogicOr]:
        """Parse an assignment expression.

        Grammar Rule:
        assignment → ( call "." )? IDENTIFIER "=" assignment
                   | logic_or ;

        Returns:
            expression: Union[Assign, Set, ParsedLogicOr]. Parsed assignment
                expression.
        """

        # "="
        expr = self.or_expr()

        if self.match(TokenType.EQUAL):
            # "="
            equals = self.previous()

            # Recursively call assignment, which will return the value
            # of the right hand
            value = self.assignment()

            if isinstance(expr, Variable):
                # Left hand is a variable
                # var foo = "bar"
                # Ensure that the left hand side is a valid assignment
                # target, then assign
                name = expr.name
                return Assign(name, value)
            elif isinstance(expr, Get):
                # Left hand is one to many get expressions for property
                # access
                # var foo = Foo()
                # foo.bar.baz = "bing"
                # Transform this get expression into a set expression
                return Set(expr.object, expr.name, value)

            self.error(equals, "Invalid assignment target.")

        return expr

    def or_expr(self) -> ParsedLogicOr:
        """Parse a logic OR expression, or any of lower precedence.

        Grammar Rule:
        logic_or → logic_and ( "or" logic_and )* ;

        Returns:
            expression: Union[ParsedLogicOr]. Parsed logic OR expression.
        """

        expr = self.and_expr()

        while self.match(TokenType.OR):
            operator = self.previous()
            right = self.and_expr()
            expr = Logical(expr, operator, right)

        return expr

    # logic_and → equality ( "and" equality )* ;
    def and_expr(self) -> Union[Logical, ParsedEquality]:
        """Parse a logic AND expression, or any of lower precedence.

        Grammar Rule:
        logic_and → equality ( "and" equality )* ;

        Returns:
            expression: Union[Logical, ParsedEquality]. Parsed logic OR expression.
        """

        expr = self.equality()

        while self.match(TokenType.AND):
            operator = self.previous()
            right = self.equality()
            expr = Logical(expr, operator, right)

        return expr

    def equality(self) -> ParsedEquality:
        """Parse an equality expression, or any of lower precedence.

        Grammar Rule:
        equality → comparison ( ( "!=" | "==" ) comparison )* ;

        Returns:
            expression: ParsedEquality. Parsed equality expression.
        """

        # comparison (1)
        expr = self.comparison()

        # * - while loop for recursion through multiple equality exprs
        # ( ( "!=" | "==" ) comparison )*
        while self.match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL):
            # ( "!=" | "==" )
            operator = self.previous()
            # comparison (2), recursing as many times as we continue to find
            # BANG_EQUAL (!=) or EQUAL_EQUAL (==)
            right = self.comparison()
            expr = Binary(expr, operator, right)

        # Return completed expression
        return expr

    # comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    def comparison(self) -> Union[Binary, ParsedUnary]:
        """Parse a comparison expression, or any of lower precedence.

        Grammar Rule:
        comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

        Returns:
            expression: Union[Binary, ParsedUnary]. Parsed comparison expression.
        """

        # term (1)
        expr = self.term()

        # ( ( ">" | ">=" | "<" | "<=" ) term )*
        while self.match(
            TokenType.GREATER,
            TokenType.GREATER_EQUAL,
            TokenType.LESS,
            TokenType.LESS_EQUAL,
        ):
            # ( ">" | ">=" | "<" | "<=" )
            operator = self.previous()
            # term (2), recursing as many times as we continue to find
            # comparison operators
            right = self.term()
            expr = Binary(expr, operator, right)

        # Return completed expression
        return expr

    def term(self) -> Union[Binary, ParsedUnary]:
        """Parse a term expression, or any of lower precedence.

        Grammar Rule:
        term → factor ( ( "-" | "+" ) factor )* ;

        Returns:
            expression: Union[Binary, ParsedUnary]. Parsed term expression.
        """

        # factor (1)
        expr = self.factor()

        # ( ( "-" | "+" ) factor )*
        while self.match(TokenType.MINUS, TokenType.PLUS):
            # ( "-" | "+" )
            operator = self.previous()
            # factor (2), recursing as many times as we continue to find factor
            # operators
            right = self.factor()
            expr = Binary(expr, operator, right)

        # Return completed expression
        return expr

    def factor(self) -> Union[Binary, ParsedUnary]:
        """Parse a factor expression, or any of lower precedence.

        Grammar Rule:
        factor → unary ( ( "/" | "*" ) unary )* ;

        Returns:
            expression: Union[Binary, ParsedUnary]. Parsed factor expression.
        """

        # unary (1)
        expr = self.unary()

        # ( ( "/" | "*" ) unary )*
        while self.match(TokenType.SLASH, TokenType.STAR):
            # ( "/" | "*" )
            operator = self.previous()
            # unary (2), recursing as many times as we continue to find unary
            # operators
            right = self.unary()
            expr = Binary(expr, operator, right)

        return expr

    def unary(self) -> ParsedUnary:
        """Parse a unary expression,r any of lower precedence.

        Grammar Rule:
        unary → ( "!" | "-" ) unary | call ;

        Returns:
            expression: ParsedUnary. Parsed unary expression.
        """

        # ( "!" | "-" ) unary
        if self.match(TokenType.BANG, TokenType.MINUS):
            # ( "!" | "-" )
            operator = self.previous()
            # unary (1), recursing as many times as we continue to find ! or -
            # e.g. !!true
            right = self.unary()
            return Unary(operator, right)

        # Nothing of lower precedence matched, check if this is a function
        # call
        return self.call()

    def finish_call(self, callee: Expr) -> Call:
        """Finish and return a call expression.

        This is called by call() and handles argument parsing. It also contains
        the zero-argument case in the call grammar rule itself.

        Grammar Rule:
        arguments → expression ( "," expression )* ;

        Args:
            callee: Expr. Expression to evaluate for the callee.

        Returns:
            expression: Call. Finished call expression.
        """

        # Continue parsing comma separated call arguments until the right paren
        # matching the left which generated this call is found
        arguments = []
        if not self.check(TokenType.RIGHT_PAREN):
            arguments.append(self.expression())
            while self.match(TokenType.COMMA):
                if len(arguments) >= 255:
                    self.error(self.peek(), "Can't have more than 255 arguments.")

                arguments.append(self.expression())

        paren = self.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")

        # Return call expression with parsed arguments added to the callee
        return Call(callee, paren, arguments)

    def call(self) -> ParsedCall:
        """Parse a call expression, or a primary.

        Grammar Rule:
        call → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;

        Returns:
            expression: ParsedCall. Parsed call expression.
        """

        # Parse the "left operand" of the function call (a primary expression)
        expr = self.primary()

        # Continue consuming calls as long as there are left parens or
        # dots for property access
        while True:
            if self.match(TokenType.LEFT_PAREN):
                # add(a, b, c)
                expr = self.finish_call(expr)
            elif self.match(TokenType.DOT):
                # foo.add(a, b, c)
                name = self.consume(
                    TokenType.IDENTIFIER, "Expect property name after '.'."
                )
                expr = Get(expr, name)
            else:
                break

        return expr

    def primary(self) -> ParsedPrimary:
        """Parse a primary expression.

        This is the lowest precedence expression and represents a concrete value.

        Grammar Rule:
        primary → "true" | "false" | "nil" | "this"
                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
                | "super" "." IDENTIFIER ;

        Returns:
            expression: ParsedPrimary. Parsed primary expression.
        """

        # "false"
        if self.match(TokenType.FALSE):
            return Literal(False)
        # "true"
        if self.match(TokenType.TRUE):
            return Literal(True)
        # "nil"
        if self.match(TokenType.NIL):
            return Literal(None)

        # NUMBER | STRING
        # e.g. 5 or "five"
        if self.match(TokenType.NUMBER, TokenType.STRING):
            return Literal(self.previous().literal)

        # "super" "." IDENTIFIER ;
        # "super" keyword, which identifies the direct superclass of the class
        # that it is being accessed in, and must be a method access
        # super.add()
        if self.match(TokenType.SUPER):
            keyword = self.previous()
            self.consume(TokenType.DOT, "Expect '.' after 'super'.")
            # return super; <- Error, can't have bare super
            method = self.consume(
                TokenType.IDENTIFIER, "Expect superclass method name."
            )

            return Super(keyword, method)

        # THIS
        # "this" keyword, which identifies the instance currently being
        # accessed during a method call on a class
        if self.match(TokenType.THIS):
            return This(self.previous())

        if self.match(TokenType.IDENTIFIER):
            return Variable(self.previous())

        if self.match(TokenType.LEFT_PAREN):
            # expression (1)
            expr = self.expression()
            # ")", if not found this is a syntax error
            self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
            # (expression)
            return Grouping(expr)

        raise self.error(self.peek(), "Expect expression.")

    def match(self, *types: TokenType) -> bool:
        """Check for a TokenType(s) and advance if present.

        Given a TokenType(s), this will check the current token against each
        one, advancing to the next token if matched.

        Args:
            types: Union[TokenType, Iterable[TokenType]]. Token types to check for.

        Returns:
            matched: bool. Whether or not the current token is one of the
                provided types.
        """

        for type in types:
            if self.check(type):
                self.advance()
                return True

        return False

    def consume(self, type: TokenType, message: str) -> Token:
        """Consume a specific type of Token, erroring if not found.

        The parser advances to the next Token, "consuming" the returned
        Token.
        This is used for things ie groupings where a "( expression" must
        have a corresponding ")".

        Args:
            type: TokenType. Type of Token to match and consume.
            message: str. Message for raised error if Token not found.

        Raises:
            ParseError: If the Token types provided are not matched.
        """

        if self.check(type):
            return self.advance()

        raise self.error(self.peek(), message)

    def check(self, type: TokenType) -> bool:
        """Check if the current token is of the provided type.

        If at the end of the file, this will always return false.

        Returns:
            result: bool. Whether or not the Token is the provided type.
        """

        if self.is_at_end():
            return False

        return self.peek().type == type

    def advance(self) -> Token:
        """Advance to the next Token and return the now previous Token.

        Returns:
            token: Token. The now previous Token after advancing to the
                next, or EOF if already at the end.
        """

        if not self.is_at_end():
            self.current += 1

        return self.previous()

    def is_at_end(self) -> bool:
        """Whether or not we are at the End-of-File (EOF) Token.

        Returns:
            at_end: bool. Whether or not parser is at EOF and finished
                parsing.
        """

        return self.peek().type == TokenType.EOF

    def peek(self) -> Token:
        """Retrieve the current Token being parsed, without advancing.

        Returns:
            token: Token. The current Token being parsed.
        """

        return self.tokens[self.current]

    def previous(self) -> Token:
        """Retrieve the previously parsed token, without advancement.

        Returns:
            previous: Token. The previously parsed (n - 1) Token.
        """

        return self.tokens[self.current - 1]

    def error(self, token: Token, message: str) -> ParseError:
        """Report and return an error encountered while parsing.

        Returns:
            error: ParseError. Error encountered while parsing.
        """

        Lox.error(token, message)
        return ParseError(token, message)

    def synchronize(self) -> None:
        """Attempt to synchronize the Parser after encountering an error.

        Synchronization in this case essentially means continuing to advance
        through Tokens until we have (probably) found the next statement to
        parse.

        This is done by looking for either the end of the current statement (;)
        or the beginning of another statement (class/fn/var/etc). This is
        not perfect, but allows for continuation of parsing subsequent lines
        if possible in order to indicate more errors to the user.
        """

        self.advance()

        while not self.is_at_end():
            if self.previous().type == TokenType.SEMICOLON:
                return

            match self.peek().type:
                case (
                    TokenType.CLASS
                    | TokenType.FUN
                    | TokenType.VAR
                    | TokenType.FOR
                    | TokenType.IF
                    | TokenType.WHILE
                    | TokenType.PRINT
                    | TokenType.RETURN
                ):
                    return
                case _:
                    self.advance()
