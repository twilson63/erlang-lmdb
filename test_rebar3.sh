#!/usr/bin/env bash
# test_rebar3.sh - Run tests with rebar3 after ensuring dependencies are built

set -e

echo "Building LMDB dependencies..."
make deps

echo "Compiling NIF..."
make compile

echo "Running rebar3 eunit..."
rebar3 eunit

echo "Tests completed successfully"
