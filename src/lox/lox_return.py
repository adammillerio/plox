#!/usr/bin/env python3
class Return(Exception):
    """Lox return "Exception"

    When the interpreter encounters a Return statement (in a valid context), it
    will raise this exception in order to unwind the stack back to the original
    call site, which can be an arbitrary distance away ie in:
    fun fib(n) {
      if (n <= 1) return n;
      return fib(n - 2) + fib(n - 1);
    }

    When this is called, the return can happen inside or outside of the if
    statement, but in both cases, it must return back to the place where the
    original call to fib() was made.

    Args:
        value: object. The return value if one was provided, this is extracted
            from the exception before continuing execution.
    """

    def __init__(self, value: object) -> None:
        self.value = value
