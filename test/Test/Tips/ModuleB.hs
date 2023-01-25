module Test.Tips.ModuleB where

import GHC.Generics

data MyType = MyType1 | MyType2 deriving stock (Show, Generic)