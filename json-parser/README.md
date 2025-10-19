# Json Parser

## Goals

- Hands-on exercize for practicing implementation of json-parser
- Understanding the practical example of `Applicative` through `newtype Parser` 

## Methods

Reference: [getting-started](https://cabal.readthedocs.io/en/stable/getting-started.html)
- To run the executable, switch into the application directory and run
  ```bash
  cabal run scratchaskell-json
  ```
- Using REPL is very useful, run the following command on terminal:
  ```bash
  cabal repl
  ```

### Copy-typing

reference: [https://www.youtube.com/watch?v=N9RUqGYuGfw]

As the initial step in writing a parser, I will replicate the above project.

## Reasoning

Writing a JSON parser from scratch (not using `parsec`) is a good idea to learn how to write a compiler using Haskell.
