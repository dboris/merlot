# Elixir Example - Word Stats

A data processing example using Elixir's Enum module from Merlot.

## Prerequisites

- Elixir must be installed alongside Erlang/OTP
- The Elixir BEAM files must be accessible at runtime

## Features

- `Enum.map`, `Enum.filter` for transformations
- `Enum.sort`, `Enum.reverse`, `Enum.take` for ordering
- `Enum.sum`, `Enum.count`, `Enum.min`, `Enum.max` for aggregations
- `Enum.any`, `Enum.all` for predicates
- Full pipe operator (`|>`) support

## Build & Run

```bash
# Compile
make

# Run
make run
```

Expected output:
```
Result: {120,[9,8,7],45,9,1,9,true,true,[10,4,16,2,18,6,14,8,12]}
```

## Results Explained

| Index | Value | Description |
|-------|-------|-------------|
| 1 | 120 | Sum of squared evens (4+16+36+64) |
| 2 | [9,8,7] | Top 3 largest |
| 3 | 45 | Sum of all |
| 4 | 9 | Count |
| 5 | 1 | Min |
| 6 | 9 | Max |
| 7 | true | Has even numbers |
| 8 | true | All positive |
| 9 | [10,4,...] | Doubled list |

## Code Highlights

```ocaml
(* Pipes work seamlessly with Elixir functions *)
let sum_of_squared_evens nums =
  nums |> Enum.filter is_even |> Enum.map square |> Enum.sum

let top_n n nums =
  nums |> Enum.sort |> Enum.reverse |> Enum.take n
```

## Elixir Enum Functions

The API follows OCaml convention (function first) for pipe compatibility:

| Function | Type | Description |
|----------|------|-------------|
| `Enum.map f list` | `('a -> 'b) -> 'a list -> 'b list` | Transform elements |
| `Enum.filter p list` | `('a -> bool) -> 'a list -> 'a list` | Keep matching |
| `Enum.sort list` | `'a list -> 'a list` | Sort ascending |
| `Enum.reverse list` | `'a list -> 'a list` | Reverse order |
| `Enum.take n list` | `int -> 'a list -> 'a list` | First n elements |
| `Enum.sum list` | `int list -> int` | Sum numbers |
| `Enum.count list` | `'a list -> int` | Count elements |
| `Enum.min list` | `'a list -> 'a` | Minimum |
| `Enum.max list` | `'a list -> 'a` | Maximum |
| `Enum.any p list` | `('a -> bool) -> 'a list -> bool` | Any matches |
| `Enum.all p list` | `('a -> bool) -> 'a list -> bool` | All match |

## Pipe Operator

The standard OCaml `|>` pipe works naturally with Elixir functions:

```elixir
# In Elixir:
numbers |> Enum.filter(&is_even/1) |> Enum.map(&square/1) |> Enum.sum()
```

```ocaml
(* In Merlot - same pattern! *)
numbers |> Enum.filter is_even |> Enum.map square |> Enum.sum
```

The compiler automatically handles argument reordering when calling Elixir.Enum.
