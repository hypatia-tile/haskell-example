# haskell-example
Store examples for learning Haskell.

## Monorepo Structure

This repository is organized as a monorepo containing multiple Haskell learning projects. Each project:
- Is created using Cabal
- Has its own directory
- Contains a README file with:
  - **Goals**: What I aim to learn or accomplish
  - **Action Steps**: The steps taken to complete the project
  - **Methods**: The techniques and tools used
  - **Reasoning**: Why I chose this approach and what I learned

## Projects

- [hello-world](./hello-world/) - A simple Hello World program to verify toolchain setup

## Building Projects

To build all projects:
```bash
cabal build all
```

To build a specific project:
```bash
cabal build hello-world
```

To run a specific project:
```bash
cabal run hello-world
```

## Adding New Projects

1. Create a new directory for your project
2. Initialize it with Cabal: `cd project-name && cabal init`
3. Add the project to `cabal.project` file
4. Create a README.md following the template structure
