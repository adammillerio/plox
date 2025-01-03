#!/usr/bin/env python3
class Return(Exception):
    def __init__(self, value: object) -> None:
        self.value = value
