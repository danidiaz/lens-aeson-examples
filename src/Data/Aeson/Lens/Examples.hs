-- |
-- Main module description goes here.
-- 

module Data.Aeson.Lens.Examples (
      -- * Pretty printing
      pp
      -- * Re-exports
      -- $reexports
    , module Data.Aeson
    , module Data.Aeson.Lens
    , module Control.Lens
    ) where

import qualified Data.ByteString.Lazy.Char8
import           Data.Text
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens
import           Control.Lens hiding ((.=)) -- conflict with a Data.Aeson operator.

-- http://hackage.haskell.org/package/haddock
-- http://haskell-haddock.readthedocs.io/en/latest/index.html

{-|
    Pretty print a 'Value' to @stdout@.
-}
pp :: Value -> IO ()
pp = Data.ByteString.Lazy.Char8.putStrLn . encodePretty

{- $reexports

    The following modules are re-exported for convenience:

    - "Data.Aeson".
    - "Data.Aeson.Lens".
    - The whole of "Control.Lens" except the ('Control.Lens.Setter..=')
      operator which conflicts with ('Data.Aeson.Types..=') from "Data.Aeson".
 
-}
