{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Example where

import Data.Conversion.Generic
import Data.Conversion.Generic.Custom
import Data.Text
import GHC.Generics
import GHC.Natural

data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool, d1 :: Natural} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: Integer, b2 :: Text, c2 :: Bool, d2 :: Integer} deriving stock (Show, Generic)
data Test2' = Test2' {b2 :: Text, a2 :: Integer, d2 :: Integer, c2 :: Bool} deriving stock (Show, Generic)

-- Convert with default implementation
deriving anyclass instance Convert Test1 Test2

unit_convertFromTest1ToTest2 :: IO ()
unit_convertFromTest1ToTest2 = print (convert Test1{a1 = 1, b1 = "test", c1 = True, d1 = 2} :: Test2)

-- Convert with user-defined implementation
instance ConvertCustom Test2 Test1 Text String where
    convertCustom _ = unpack

instance (Num a) => ConvertCustom Test2 Test1 Integer a where
    convertCustom _ = fromInteger

deriving anyclass instance ConvertCustom Test2 Test1 Test2 Test1
deriving via (ConvertCustomType Test1) instance Convert Test2 Test1

unit_convertFromTest2ToTest1 :: IO ()
unit_convertFromTest2ToTest1 = print (convert Test2{a2 = 1, b2 = pack "test", c2 = True, d2 = 2} :: Test1)

-- Swap only the order of records between data types with the same field names
instance Convert Test2 Test2' where
    convert Test2{..} = Test2'{..}

instance Convert Test1 Test2' where
    convert = convert @Test2 . convert

unit_convertFromTest1ToTest3 :: IO ()
unit_convertFromTest1ToTest3 = print (convert Test1{a1 = 1, b1 = "test", c1 = True, d1 = 2} :: Test2')

-- Convert between isomorphic sum types
data AB = A | B deriving (Show, Generic)

deriving anyclass instance Convert AB Bool

unit_convertFromABToBool :: IO ()
unit_convertFromABToBool = print A >> print (convert A :: Bool)

deriving anyclass instance Convert Bool AB

unit_convertFromBoolToAB :: IO ()
unit_convertFromBoolToAB = print True >> print (convert True :: AB)