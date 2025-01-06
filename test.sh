#!/usr/bin/env bash
ACTION=${1:-'test'}

function clone_repo() {
    if ! [ -d "craftinginterpreters" ]; then
        git clone 'https://github.com/munificent/craftinginterpreters.git'
    fi
}

function build_test() {
    clone_repo

    cd 'craftinginterpreters'

    make get
    make debug build/test.dart.snapshot

    cd ..
}

function test_plox() {
    cd 'craftinginterpreters'

    dart 'build/test.dart.snapshot' -i "../plox.sh" jlox

    cd ..
}

function test() {
    build_test
    test_plox
}

${ACTION}

