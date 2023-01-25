{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.TH.Example where

import Data.Coerce (coerce)
import Data.Generic.Conversion
import Data.Generic.Conversion.TH
import Data.Kind
import Data.Text
import GHC.Generics
import GHC.Natural

newtype NewInteger = NewInteger Integer deriving stock (Show)

data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool, d1 :: Natural} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: NewInteger, b2 :: Text, c2 :: Bool, d2 :: NewInteger} deriving stock (Show, Generic)

-- `deriveConvertFromAnyclass` also works (requires `DeriveAnyClass` instead of `DerivingVia`)
deriveConvert
    ''Test1
    ''Test2
    [ [|coerce . toInteger :: Integral a => a -> NewInteger|]
    , [|pack :: String -> Text|]
    ]

unit_convertFromTest1ToTest2TH :: IO ()
unit_convertFromTest1ToTest2TH = print (convert (Test1{a1 = 1, b1 = "a", c1 = True, d1 = 1}) :: Test2)

deriveConvert
    ''Test2
    ''Test1
    [ [|fromInteger . coerce :: forall (a :: Type). Num a => NewInteger -> a|]
    , [|unpack :: Text -> String|]
    ]

unit_convertFromTest2ToTest1TH :: IO ()
unit_convertFromTest2ToTest1TH = print (convert (Test2{a2 = NewInteger 1, b2 = pack "a", c2 = True, d2 = NewInteger 1}) :: Test1)
