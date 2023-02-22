module Test.Examples.Wrap.ModuleB where

import Data.Text
import GHC.Generics

data MyType = MyType {a :: Int, b :: Text} deriving stock (Show, Generic)