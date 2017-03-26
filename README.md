# Hindley-Milner type inference system in Haskell

## Building and running

Running the binary without an argument starts the REPL.

```
ghc Main.hs 
./Main

# or

runhaskell Main.hs
```

Some examples are in `examples/expressions.fun`. Load them by specifying the filename.

```
runhaskell Main.hs examples/expressions.fun
```

## Syntax 

Small functional language from the course materials of CS5218 Program Analysis at NUS.

![Imgur](http://i.imgur.com/gXRmhTe.png?1)
