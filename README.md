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

Warning: apparently doing two "imports" of the same name
in the same ghci session may will cause some strange error to occur.
I have no idea what the implications of this are yet,
but don't go trying to use this library in mission-critical code
until the record system extensions it relies on,
and the particular ways we are abusing them,
are more carefully inspected.
I believe this may be connected to the way that
I've punned "Import" to be the qualified name
for all of the modules. This appears to only be a problem
in ghci; using `let Import.Foo{blah} = def in baz` 
always seems to work as expected in source files.

----

One of the important next steps for this library
is going to be figuring out how to shorten
that ghastly long import list.
I'm thinking either TH, or a custom pragma
that postprocesses your source files, adding the imports to them.
But I'd really like this whole environment to be bundled
and easily accessible, without having to copy/paste 40 lines of code.

I've currently modeled the "modules" after ClassyPrelude,
though I think that in the future I'll just include
the entirety of the a module in
its corresponding first-class representation.
That could be a lot of manual labor,
so it would also be nice to write some code
to generate such first-class modules automatically.
