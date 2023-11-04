# Qiqe Documentation

## Table of Contents

- [Qiqe Documentation](#qiqe-documentation)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Syntax](#syntax)
    - [Comments](#comments)
    - [Variables](#variables)
    - [Functions](#functions)
      - [Currying](#currying)
    - [Operators](#operators)
    - [Control Flow](#control-flow)
  - [Contributing in the design](#contributing-in-the-design)

## Introduction

Qiqe is a general purpose functional language. It is inspired by Haskell, Elm and Lisp. It is a compiled language that compiles to javascript. It is designed to be used in the browser.

## Syntax

### Comments

```qiqe
# This is a comment
```

### Variables

```qiqe
# Variables are immutable
let x = 1
```

### Functions

```qiqe
# Functions inspired by Lambda Calculus
let add = \x y. x + y
```

#### Currying

```qiqe
# Currying
let add = \x y. x + y
let succ = add 1

succ 2 # Returns 3
```

### Operators

> **TODO**: Operators are not implemented yet. Only the operators `>>`, `<<`, `|>` and `<|` are implemented, and the others operators are implemented as functions.

### Control Flow

```qiqe
# If
if x == 1
  then 1
  else 2
```

## Contributing in the design

If you want to contribute in the design of the language, you can open an issue or a pull request. You can also contact me at [Twitter](https://twitter.com/fabianmativeal).
