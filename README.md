# jsp

A modular and highly efficient language processor for *MyJS* (a custom scripting language), featuring robust recovery, rich diagnostics and optional per-module trace output for debugging and analysis.

![diagnostic example](.github/assets/cover.png)

## Table of Contents
- [Features](#features)
- [How to use](#how-to-use)
- [Benchmark](#benchmark)
- [Installation](#installation)

## Features

### Error recovery

### Fix suggestions

## How to use

To get an overview of all available options run `jsp -h` or `jsp --help`.

### Basic use

To process one or more files just run `jsp <FILE> [<FILE> [...]]`
Every file will be processed even if some of them generate errors.

Add the `--quiet` or `-q` to discard the output if you only need the return value.

### Showing traces

You can inspect what each stage of the processing generates by enabling one or more trace flags:

`--lexer-trace [<FILE>]` or `-l [<FILE>]` — dump the generated tokens

`--parse-trace [<FILE>]` or `-p [<FILE>]` — dump the parser output / grammar trace

`--symtb-trace [<FILE>]` or `-s [<FILE>]` — dump the generated symbol table

Each trace is written to the given file. If no file is provided, it prints to `stdout`.

## Benchmark

## Installation
## Installation

### Requirements
- [Rust](https://www.rust-lang.org/tools/install) (latest stable)

### Build from source
```bash
git clone https://github.com/suuniqo/jsp.git
cd jsp
cargo build --release
