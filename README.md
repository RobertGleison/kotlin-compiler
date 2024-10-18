# kotlin-compiler
Kotlin compiler for university compilers project

## Notas: 
Tem que ter instalado o alex.

1. Transformar o código do alex em haskell

``` 
alex lexer.x 
```


2. Compilar o código haskell
```
ghc --make lexer.hs"
```



3. Rodar o lexer:

```
./lexer < examples/example1.kt
```