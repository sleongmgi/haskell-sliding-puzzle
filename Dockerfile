FROM haskell:latest

RUN cabal update && cabal install random
