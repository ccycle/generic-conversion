module Test.Examples.Wrap.ModuleA where

import GHC.Generics

data MyType = MyType {a :: Integer, b :: String} deriving stock (Show, Generic)
