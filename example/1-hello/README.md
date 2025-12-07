# 1-hello

This example demonstrates the OCaml features that Merlot can compile to
BEAM/Erlang.

## Features demonstrated

- **Basic functions and arithmetic** - `add`, `double`, `square`
- **Float operations** - `circle_area` with `*.` operator
- **Recursive functions** - `factorial`, `fibonacci`
- **List operations** - `sum_list`, `length`, `map_double` with pattern matching
- **Tuples** - `make_point`, `add_points`
- **Custom variants (ADTs)** - `color`, `shape` types with constructors
- **Option type** - `safe_divide` returning `Some`/`None`
- **Erlang FFI** - external declarations calling `erlang:self/0` and `lists:reverse/1`

## Build & Run

```bash
cd example/1-hello

# Compile to BEAM
make

# Run tests
make test
```

## Expected output

```
add(2, 3) = 5
factorial(5) = 120
fibonacci(10) = 55
sum_list([1,2,3,4]) = 10
map_double([1,2,3]) = [2,4,6]
color_to_string(red) = "red"
area({circle, 2.0}) = 12.56636
safe_divide(10, 2) = {some,5.0}
reverse_list([1,2,3]) = [3,2,1]
```

## How it maps to Erlang

| OCaml | Erlang |
|-------|--------|
| `let f x = ...` | `'f'/1 = fun (X) -> ...` |
| `[1; 2; 3]` | `[1\|[2\|[3\|[]]]]` |
| `(a, b)` | `{A, B}` |
| `Some x` | `{some, X}` |
| `None` | `none` |
| `Red` | `red` |
| `Circle r` | `{circle, R}` |
| `external f : t = "mod" "fun"` | `call 'mod':'fun'(...)` |
