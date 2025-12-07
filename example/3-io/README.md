# I/O Example - Greeting

An interactive greeting program demonstrating I/O operations in Merlot.

## Features

- Printing to stdout with `Io.put_chars`
- Reading input with `Io.get_line`
- String trimming with `String.trim`
- String concatenation with `^`

## Build & Run

```bash
# Compile
make

# Run interactively
make run
```

## Example Session

```
================================
  Welcome to Merlot on BEAM!
================================

What is your name? Alice

Hello, Alice!
Nice to meet you, Alice!

Goodbye!
```

## I/O Functions

| Function | Description |
|----------|-------------|
| `Io.put_chars s` | Print string without newline |
| `Io.get_line prompt` | Read a line from stdin |
| `Io.format fmt args` | Printf-style output (Erlang format) |

## String Functions

| Function | Description |
|----------|-------------|
| `String.trim s` | Strip leading/trailing whitespace |
| `String.uppercase s` | Convert to uppercase |
| `String.lowercase s` | Convert to lowercase |
| `String.length s` | Get string length |
