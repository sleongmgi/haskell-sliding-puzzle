FROM haskell:latest

RUN cabal update && cabal install random

ENV HASKELL_SLIDING_PUZZLE /haskell-sliding-puzzle
COPY . $HASKELL_SLIDING_PUZZLE
WORKDIR $HASKELL_SLIDING_PUZZLE
RUN stack --allow-different-user build
CMD ["stack", "exec", "haskell-sliding-puzzle-exe"]
