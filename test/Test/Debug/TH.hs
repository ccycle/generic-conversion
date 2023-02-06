{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Debug.TH where

import Data.Generic.Conversion
import Data.Text
import GHC.Generics
import GHC.Natural
import GHC.TypeLits
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

unit_decsConvert :: IO ()
unit_decsConvert = runQ decsConvert >>= print

decsConvertDerivingVia :: Q [Dec]
decsConvertDerivingVia =
    [d|
        deriving via (FromGeneric Test1 Test2) instance ConvertCustom Test1 Test2 Test1 Test2

        deriving via (FromCustom Test1 Test2) instance Convert Test1 Test2
        |]

unit_decsConvertDerivingVia :: IO ()
unit_decsConvertDerivingVia = runQ decsConvertDerivingVia >>= print

decsConvertAnyclass :: Q [Dec]
decsConvertAnyclass =
    [d|
        deriving anyclass instance ConvertCustom Test1 Test2 Test1 Test2

        deriving anyclass instance Convert Test1 Test2
        |]

unit_decsConvertAnyclass :: IO ()
unit_decsConvertAnyclass = runQ decsConvertAnyclass >>= print

data AB = A | B deriving (Generic)

type ConNamesOrderErrorMessage a b =
    'Text "The orders of constructor names do not match: " ':<>: 'ShowType a ':<>: 'Text ", " ':<>: 'ShowType b

decsConvertTypeError :: Q [Dec]
decsConvertTypeError =
    [d|
        instance Convert Bool AB where
            convert = error "unreachable" :: (TypeError (ConNamesOrderErrorMessage Bool AB)) => Bool -> AB
        |]

unit_decsConvertTypeError :: IO ()
unit_decsConvertTypeError = runQ decsConvertTypeError >>= print
