Give it a spin! Make sure you have the latest containers, unordered-containers, etc.

    $ cabal install classy-prelude
    $ git clone git://github.com/DanBurton/modular-prelude.git
    $ cd modular-prelude
    $ ghci -XRecordWildCards -XNamedFieldPuns ModularPrelude/Import.hs

More details soon to come.

But try this:

    ghci> let Import.Map{empty,insert} = From._Data_Map_
    ghci> :t empty

No, most of it doesn't rely on classy-prelude.
Sorry for forcing that dependency on you,
but check it out:

    ghci> let Import.Classy{..} = From._ClassyPrelude_Classes_

The right-hand side of the imports doesn't have to be that verbose.
Just use `def` if that suits you.

    ghci> let Import.T{length, filter, pack} = def
    ghci> length $ filter Data.Char.isAlpha $ pack "Hello, world!"

