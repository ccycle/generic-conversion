{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.TH.Declare where

import Data.Conversion.Generic
import Data.Conversion.Generic.Custom
import Data.Text
import GHC.Generics
import GHC.Natural
import Language.Haskell.TH

data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool, d1 :: Natural} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: Integer, b2 :: Text, c2 :: Bool, d2 :: Integer} deriving stock (Show, Generic)

-- Debug for AST
decsConvert :: Q [Dec]
decsConvert =
    [d|
        instance Integral a => ConvertCustom Test1 Test2 a Integer where
            convertCustom _ = toInteger
        |]

decsConvertGeneric :: Q [Dec]
decsConvertGeneric =
    [d|
        deriving anyclass instance ConvertCustom Test1 Test2 Test1 Test2

        deriving via (ConvertCustomType Test2) instance Convert Test1 Test2
        |]

expConvertFunc :: Q Exp
expConvertFunc =
    [|toInteger :: forall (a :: Type). Integral a => a -> Integer|]

unit_decsConvert :: IO ()
unit_decsConvert = runQ decsConvert >>= print

unit_expConvertFunc :: IO ()
unit_expConvertFunc = runQ expConvertFunc >>= print

unit_decsConvertGeneric :: IO ()
unit_decsConvertGeneric = runQ decsConvertGeneric >>= print