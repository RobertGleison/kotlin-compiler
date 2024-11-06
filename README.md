# Kotlin Compiler
A Kotlin compiler implementation created for the Compilers course.

## 1. Prerequisites
- GHC (Glasgow Haskell Compiler) to run Haskell code
- Cabal for dependency management
- Alex lexer generator to create the lexical analyzer
- Happy parser generator to create the Abstract Syntax Tree (AST)

### Installing GHC and Cabal


 For Linux/Ubuntu:
```bash
# Add the repository
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y ghc cabal-install
```


### Installing Alex and Happy
Make sure you have both Alex and Happy packages installed. If not, follow these installation methods:

1. Using Cabal:
```bash
cabal update
cabal install alex
cabal install happy
```

### Verifying installation:
```
ghc --version
cabal --version
alex --version
happy --version
```

## 2. Build and Run

1. Generate the lexical analyzer from Alex specification:
```
alex Lexer.x -o Lexer.hs
```

2. Generate the parser generator from Happy specification: (Not working yet)
```
happy Parser.y -o Parser.hs
```

3. Run the compiler with example files:
```
ghc Main.hs -o Main
./Main examples/example1.kt
```