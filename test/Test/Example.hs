{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Example where

import Data.Conversion.Generic
import Data.Text
import GHC.Generics

data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: Integer, b2 :: Text, c2 :: Bool} deriving stock (Show, Generic)

data AB = A | B deriving stock (Show, Generic)

instance Convert Int Integer where
    convert = toInteger

instance Convert String Text where
    convert = pack

deriving anyclass instance Convert Test1 Test2
deriving anyclass instance Convert AB Bool
deriving anyclass instance Convert Bool AB

unit_convertTest1ToTest2 :: IO ()
unit_convertTest1ToTest2 = print (convert (Test1 1 "a" True) :: Test2)

unit_convertABToBool :: IO ()
unit_convertABToBool = print (convert A :: Bool) >> print (convert B :: Bool)

unit_convertBoolToAB :: IO ()
unit_convertBoolToAB = print (convert True :: AB) >> print (convert False :: AB)
