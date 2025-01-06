#!/usr/bin/env bash
SCRIPT_DIR=$(dirname "${BASH_SOURCE[0]}")

uv run --project "${SCRIPT_DIR}" plox ${@}
