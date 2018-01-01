# What's this?

A Haskell package containing additional examples for
[lens-aeson](http://hackage.haskell.org/package/lens-aeson). It might be
helpful for people who are not yet very familiar with
[lens](https://hackage.haskell.org/package/lens).

# How to use this package?

Simple read the haddock documentation.

You can also play with the examples in the *repl*, by downloading the [Git
repo](https://github.com/danidiaz/lens-aeson-examples), building it, and
invoking

    $ cabal repl lens-aeson-examples
    Preprocessing library for lens-aeson-examples-1.0.0.0..
    GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
    Loaded GHCi configuration from /root/.ghci
    [1 of 1] Compiling Data.Aeson.Lens.Examples ( src/Data/Aeson/Lens/Examples.hs, interpreted )
    Ok, one module loaded.
    *Data.Aeson.Lens.Examples> pp Null -- we have Data.Aeson, Data.Aeson.Lens and Control.Lens in scope
    null

