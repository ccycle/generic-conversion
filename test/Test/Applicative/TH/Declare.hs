{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Applicative.TH.Declare where

import Data.Generic.Conversion.Applicative
import Data.Text
import GHC.Generics
import GHC.Natural
import Language.Haskell.TH

data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool, d1 :: Natural} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: Integer, b2 :: Text, c2 :: Bool, d2 :: Integer} deriving stock (Show, Generic)

-- Debug for AST
decsConvertM :: Q [Dec]
decsConvertM =
    [d|
        instance Integral a => ConvertCustomM m Test1 Test2 a Integer where
            convertCustomM _ = pure . toInteger
        |]

decsDeriveConvertCustomM =
    [d|
        deriving anyclass instance (Applicative m) => ConvertCustomM m Test1 Test2 Test1 Test2
        |]

decsDeriveConvertM =
    [d|
        deriving anyclass instance (Applicative m) => ConvertM m Test1 Test2
        |]

expConvertMFunc :: Q Exp
expConvertMFunc =
    [|pure . toInteger :: (Applicative m, Integral a) => a -> m Integer|]

expConvertMFuncForall :: Q Exp
expConvertMFuncForall =
    [|pure . toInteger :: forall (a :: Type) m. Integral a => a -> m Integer|]

unit_decsConvertM :: IO ()
unit_decsConvertM = runQ decsConvertM >>= print

unit_expConvertMFunc :: IO ()
unit_expConvertMFunc = runQ expConvertMFunc >>= print

unit_expConvertMFuncForall :: IO ()
unit_expConvertMFuncForall = runQ expConvertMFuncForall >>= print

unit_decsDeriveConvertM :: IO ()
unit_decsDeriveConvertM = runQ decsDeriveConvertM >>= print

unit_decsDeriveConvertCustomM :: IO ()
unit_decsDeriveConvertCustomM = runQ decsDeriveConvertCustomM >>= print