# Hello World

## Goals
- Learn the basic structure of a Haskell program
- Understand how to set up a simple executable project with Cabal
- Practice building and running Haskell programs

## Action Steps
1. Initialize a Cabal project with executable target
2. Write a simple main function that prints "Hello, Haskell!"
3. Build the project using Cabal
4. Run the executable

## Methods
- Using `cabal init` to create a new project structure
- Writing Haskell code in `app/Main.hs`
- Building with `cabal build`
- Running with `cabal run`

## Reasoning
This is the simplest possible Haskell program, serving as a foundation for learning. By starting with a "Hello World" program, we can:
- Verify that the Haskell toolchain is correctly installed
- Understand the basic project structure (cabal file, source directories)
- Learn the minimal requirements for a Haskell program (main function with IO type)
- Practice the build and run workflow that will be used for more complex projects

## Build and Run
```bash
cabal build
cabal run hello-world
```
