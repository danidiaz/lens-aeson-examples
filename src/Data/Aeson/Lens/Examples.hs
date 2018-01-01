-- |
-- @lens-aeson@ doesn't provide 'Lens'es or 'Setter's, but rather 'Prism's
-- and 'Traversal's. The reason is that 'Value' is a sum type, and we can't be
-- sure that values always have some particular field present. (Compare with
-- tuples, which always have two components that can be targeted with '_1' and '_2'.)
--
-- The examples in this module require the @OverloadedStrings@ extension.

{-# LANGUAGE OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}
module Data.Aeson.Lens.Examples (
      -- * Searching inside a Value
      persons
      -- $searching

      -- * Pretty printing
    , pp
      -- * Re-exports
      -- $reexports
    , module Data.Aeson
    , module Data.Aeson.Lens
    , module Control.Lens
    ) where

import qualified Data.ByteString.Lazy.Char8
import           Data.Text
import           Data.Map
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens
import           Control.Lens hiding ((.=)) -- conflict with a Data.Aeson operator.
import           GHC.Generics

-- http://hackage.haskell.org/package/haddock
-- http://haskell-haddock.readthedocs.io/en/latest/index.html
-- https://github.com/sol/doctest#readme

data Pet = Dog 
         | Cat 
         deriving (Show,Eq,Generic)  
instance ToJSON Pet

data Hobby = Surfing
           | Running
           | Reading
           | Cooking
           deriving (Show,Eq,Generic)   
instance ToJSON Hobby

data Person = Person 
            { name :: Text
            , age :: Int
            , hobbies :: [Hobby]
            , pets :: Map Text Pet 
            } deriving (Show,Eq,Generic)    
instance ToJSON Person

{- $setup
>>> :set -XOverloadedStrings
-}

{-| 

>>> pp persons  
[
    {
        "age": 43,
        "name": "Alice",
        "pets": {
            "Fido": "Dog",
            "Luna": "Cat"
        },
        "hobbies": [
            "Running",
            "Reading"
        ]
    },
    {
        "age": 50,
        "name": "Bob",
        "pets": {},
        "hobbies": [
            "Surfing",
            "Cooking",
            "Reading"
        ]
    },
    {
        "age": 51,
        "name": "Jim",
        "pets": {
            "Pluto": "Dog"
        },
        "hobbies": []
    }
]

-}
persons :: Value
persons = 
    toJSON $
    [ Person "Alice" 
             43 
             [Running,Reading]
             (Data.Map.fromList [("Fido",Dog),("Luna",Cat)])
    , Person "Bob" 
             50 
             [Surfing,Cooking,Reading]
             Data.Map.empty
    , Person "Jim" 
             51 
             []
             (Data.Map.fromList [("Pluto",Dog)])
    ]

{- $searching

= Getting the names of all persons

>>> persons^..values.key "name"._String
["Alice","Bob","Jim"]

= Getting the name of the second person

>>> persons^..nth 1.key "name"._String
["Bob"]

= Getting the names of all persons not named Bob.

>>> persons^..values.key "name"._String.filtered (hasn't (only "Bob"))
["Alice","Jim"]

>>> persons^..values.key "name"._String.filtered (\name -> name /= "Bob")
["Alice","Jim"]

>>> persons^..values.key "name"._String.filtered (hasn't (only "Bob")).each
"AliceJim"

= Getting the ages of all persons not named Bob.

Here the situation is a bit different: we are filtering depending on a value
(the person's name) we are not going to extract.

>>> persons^..values.filtered (hasn't (key "name"._String.only "Bob")).key "age"._Integer
[43,51]

Notice that we now filter on the person objects *directly*, and move the search
for the name *inside* the filtering condition. Then we extract the ages from the
filtered person objects.

>>> :{
    let noBob person = case preview (key "name"._String) person of
            Just name | name == "Bob" -> False
            _ -> True
    in persons^..values.filtered noBob.key "age"._Integer
    :}
[43,51]

-}

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


