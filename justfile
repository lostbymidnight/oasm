# List all commands
default:
    @just --list

# Build everything
build:
    dune build

# Run an .oasm program
run file:
    dune exec oasm -- {{file}}

# Run tests
test:
    dune test

# Accept the current test output as the new expected output
promote:
    dune test --auto-promote

# Remove build output
clean:
    dune clean

# Rebuild on file changes
watch:
    dune build --watch
