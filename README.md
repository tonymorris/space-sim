# Space Simulator

A Haskell client for [spacerace](https://github.com/lmccalman/spacerace), written for the NICTA ETD retreat (winter 2015).

## Developing

To run this client you'll need [zeromq](http://zeromq.org/area:download) and [GHC 7.10](https://www.haskell.org/downloads) installed.
Then simply `cabal install --only-dep && cabal repl` to load the code, and run `main` to try the client.

If you use Docker:

```sh
$ docker run -ti --name spacerace -v $(pwd):/code -w /code haskell:7.10 bash
# apt-get update && apt-get install libzmq3-dev pkg-config -y
# cabal update && cabal sandbox init && cabal install --only-dep
```

![NICTA](http://i.imgur.com/Ns5hntl.jpg)
