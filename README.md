Give it a spin! Make sure you have the latest containers, unordered-containers, etc.

    $ cabal install classy-prelude
    $ ghci -XRecordWildCards -XNamedFieldPuns ModularPrelude/Import.hs

More details soon to come.

But try this:

    ghci> let Import.Map{empty,insert} = From._Data_Map_
    ghci> :t empty

