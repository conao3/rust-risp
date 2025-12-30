# risp

A minimal Lisp interpreter written in Rust.

[![License: MIT](https://img.shields.io/github/license/conao3/rust-risp.svg?style=flat-square)](https://github.com/conao3/rust-risp/blob/master/LICENSE)
[![Release](https://img.shields.io/github/tag/conao3/rust-risp.svg?style=flat-square)](https://github.com/conao3/rust-risp/releases)

## Overview

risp is a lightweight Lisp implementation built from scratch in Rust. It provides an interactive REPL with command history and supports essential Lisp features including lambda expressions, arithmetic operations, and comparison predicates.

## Features

- Interactive REPL with persistent command history
- Core data types: booleans, symbols, numbers, lists, and functions
- Lambda expressions with lexical scoping
- Built-in arithmetic operators: `+`, `-`, `*`, `/`
- Comparison predicates: `>`, `<`, `>=`, `<=`, `=`

## Installation

### From Source

```bash
git clone https://github.com/conao3/rust-risp.git
cd rust-risp
cargo build --release
```

The binary will be available at `target/release/risp`.

## Usage

Start the REPL:

```bash
cargo run
```

Example session:

```lisp
risp> (+ 1 2 3)
6
risp> (def x 10)
10
risp> (* x 2)
20
risp> (fn (a b) (+ a b))
Lambda {}
```

## Project Structure

```
src/
  main.rs    - Entry point and REPL initialization
  lib.rs     - Module exports
  types.rs   - Core data types (RispExp, RispErr, RispEnv)
  parser.rs  - S-expression parser
  eval.rs    - Expression evaluator
  env.rs     - Environment and built-in functions
  repl.rs    - REPL implementation
```

## Contributing

Contributions are welcome. Feel free to open issues for bug reports or feature requests, and submit pull requests for improvements.

## License

MIT License - Copyright (c) Naoya Yamashita

See [LICENSE](https://github.com/conao3/rust-risp/blob/master/LICENSE) for details.

## Author

Naoya Yamashita ([@conao3](https://github.com/conao3))
