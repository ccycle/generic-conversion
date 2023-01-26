module Test.Examples.Wrap.ModuleA where

import GHC.Generics

data MyType = MyType1 | MyType2 deriving stock (Show, Generic)
