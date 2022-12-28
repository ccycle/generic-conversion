{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.TH.Example where

import Data.Coerce (coerce)
import Data.Conversion.Generic
import Data.Conversion.Generic.TH
import Data.Text
import GHC.Generics

-- Test for conversion
data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: Integer, b2 :: Text, c2 :: Bool} deriving stock (Show, Generic)

deriveConvert ''Test1 ''Test2 [[|toInteger :: Int -> Integer|], [|pack :: String -> Text|]]

unit_convertFromTest1ToTest2 :: IO ()
unit_convertFromTest1ToTest2 = print (convert (Test1 1 "a" True) :: Test2)

-- Test for bidirectional conversion
newtype NewInt = NewInt Int deriving stock (Show)
newtype NewInteger = NewInteger Integer deriving stock (Show)
data Test3 = Test3 {a3 :: NewInt, b3 :: String, c3 :: Bool} deriving stock (Show, Generic)
data Test4 = Test4 {a4 :: NewInteger, b4 :: Text, c4 :: Bool} deriving stock (Show, Generic)

deriveBidirectionalConvert
    ''Test3
    ''Test4
    [ [|coerce . toInteger @Int . coerce :: NewInt -> NewInteger|]
    , [|coerce . fromInteger @Int . coerce :: NewInteger -> NewInt|]
    , [|pack :: String -> Text|]
    , [|unpack :: Text -> String|]
    ]

unit_convertFromTest3ToTest4 :: IO ()
unit_convertFromTest3ToTest4 = print (convert (Test3 (NewInt 1) "a" True) :: Test4)
unit_convertFromTest4ToTest3 :: IO ()
unit_convertFromTest4ToTest3 = print (convert (Test4 (NewInteger 1) (pack "a") True) :: Test3)
