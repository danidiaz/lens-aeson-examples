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
>>> import Data.Monoid
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

== Getting the names of all persons

>>> persons^..values.key "name"._String
["Alice","Bob","Jim"]

== Getting the name of the second person

>>> persons^..nth 1.key "name"._String
["Bob"]

== Getting the names of all persons not named Bob.

We use 'filtered' in together with 'hasn't' and 'only'.

'has' and 'hasn't' are useful combinators that check if a 'Fold' hits or does
not hit any targets.

'only' is a strange 'Prism' that only matches in the case of equality,
returning an uninformative @()@. But this is enough to use it with 'has' and
'hasn't'.

>>> persons^..values.key "name"._String.filtered (hasn't (only "Bob"))
["Alice","Jim"]

We can also use simple functions as arguments to 'filtered':

>>> persons^..values.key "name"._String.filtered (\name -> name /= "Bob")
["Alice","Jim"]

The nice thing about 'filtered' is that it doen't yank us out of the lensy
world; we can keep composing with 'Fold's or 'Traversal's. Here we compose with
'each', a traversal which lets us visit the elemets of monomorphic containers
like 'Text':

>>> persons^..values.key "name"._String.filtered (hasn't (only "Bob")).each
"AliceJim"

== Getting the ages of all persons not named Bob.

Here the situation is a bit different: we are filtering depending on a value
(the person's name) that we do not want to extract.

>>> persons^..values.filtered (hasn't (key "name"._String.only "Bob")).key "age"._Integer
[43,51]

Notice that we now filter on the person objects /directly/, and move the search
for the name /inside/ the filtering condition. Then we extract the ages from the
filtered person objects.

>>> :{
    let noBob person = case preview (key "name"._String) person of
            Just name | name == "Bob" -> False
            _ -> True
    in persons^..values.filtered noBob.key "age"._Integer
    :}
[43,51]

>>> persons^..values.filtered (noneOf (key "name"._String) (=="Bob")).key "age"._Integer
[43,51]

== Getting the names of all persons who like reading or cooking

'Fold's can be pasted togeter using ('Data.Monoid.<>'), which is sometimes useful:

>>> persons^..values.filtered (has (key "hobbies".values._String.(only "Reading"<>only "Cooking"))).key "name"._String
["Alice","Bob"]

Another way of saying the same, using the 'anyOf' combinator:

>>> persons^..values.filtered (anyOf (key "hobbies".values._String) (\t -> t=="Reading" || t=="Cooking")).key "name"._String
["Alice","Bob"]

== Getting the names of all persons whose every pet is a dog 

>>> persons^..values.filtered (allOf (key "pets".members._String) (=="Dog")).key "name"._String
["Bob","Jim"]

== Getting the names of all pets

Notice that the pet names are /keys/ in an object.

'members' is an 'IndexedTraversal', and we can compose it with 'asIndex' to
extract the object keys:

>>> persons^..values.key "pets".members.asIndex
["Fido","Luna","Pluto"]

== Getting the types of pets not named Luna

Filtering based on object keys is done using the 'indices' function.

>>> persons^..values.key "pets".members.indices (/="Luna")._String
["Dog","Dog"]

'values' is also an 'IndexedTraversal'; the index is the position on the
array. We can filter based on that:

>>> persons^..values.indices even.key "name"._String
["Alice","Jim"]

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


