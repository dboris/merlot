# List Processing Example - Data Pipeline

A functional data processing pipeline demonstrating list operations in Merlot.

## Features

- Higher-order functions: `Lists.map`, `Lists.filter`, `Lists.foldl`
- List operations: `Lists.sort`, `Lists.reverse`, `Lists.sublist`
- Predicates: `Lists.any`, `Lists.all`, `Lists.member`
- Passing functions as arguments

## Build & Run

```bash
# Compile
make

# Run
make run
```

Expected output:
```
Result: {120,[9,8,7],45,9,[10,4,16,2,18,6,14,8,12],true,false}
```

## Results Explained

The tuple contains:
| Index | Value | Description |
|-------|-------|-------------|
| 1 | 120 | Sum of squared even numbers (4+16+36+64) |
| 2 | [9,8,7] | Top 3 largest numbers |
| 3 | 45 | Sum of all numbers |
| 4 | 9 | Count of elements |
| 5 | [10,4,...] | Each element doubled |
| 6 | true | List contains 5 |
| 7 | false | List doesn't contain 10 |

## Code Highlights

```ocaml
(* Filter evens, square them, sum the result *)
let process_evens nums =
  let evens = Lists.filter is_even nums in
  let squared = Lists.map square evens in
  Lists.foldl add 0 squared

(* Get top N largest numbers *)
let top_n n nums =
  let sorted = Lists.sort nums in
  let reversed = Lists.reverse sorted in
  Lists.sublist reversed n
```

## List Functions Used

| Function | Description |
|----------|-------------|
| `Lists.map f xs` | Apply f to each element |
| `Lists.filter p xs` | Keep elements where p is true |
| `Lists.foldl f acc xs` | Left fold over list |
| `Lists.sort xs` | Sort in ascending order |
| `Lists.reverse xs` | Reverse list |
| `Lists.sublist xs n` | Take first n elements |
| `Lists.length xs` | Get list length |
| `Lists.member x xs` | Check if x is in list |
| `Lists.any p xs` | True if any element matches p |
| `Lists.all p xs` | True if all elements match p |
