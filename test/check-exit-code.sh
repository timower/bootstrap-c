#!/bin/bash
# Helper script to check that a program exits with a specific code
# Usage: check-exit-code.sh <expected_code> <program> [args...]

expected_code=$1
shift

# Run the program and capture its exit code
"$@"
actual_code=$?

# Check if the exit code matches
if [ $actual_code -eq $expected_code ]; then
    echo "✓ Program exited with expected code $expected_code"
    exit 0
else
    echo "✗ Program exited with code $actual_code, expected $expected_code"
    exit 1
fi