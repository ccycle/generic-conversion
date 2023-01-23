{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Test.Applicative.Examples where

import Control.Exception.Safe
import Data.Coerce
import Data.Generic.Conversion
import Data.Generic.Conversion.Applicative
import Data.Text
import GHC.Generics
import GHC.Natural

data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool, d1 :: Natural} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: Integer, b2 :: Text, c2 :: Bool, d2 :: Integer} deriving stock (Show, Generic)

data NegativeException = NegativeException deriving stock (Eq, Show)
instance Exception NegativeException

checkIfPositive :: (Ord a, Num a, MonadThrow f) => a -> f a
checkIfPositive a = if a >= 0 then pure a else throwM NegativeException

instance MonadThrow m => ConvertCustomM m Test1 Test2 Int Integer where
    convertCustomM _ = checkIfPositive . fromIntegral
instance MonadThrow m => ConvertCustomM m Test1 Test2 String Text where
    convertCustomM _ = pure . pack
instance MonadThrow m => ConvertCustomM m Test1 Test2 Bool Bool where
    convertCustomM _ = pure
instance MonadThrow m => ConvertCustomM m Test1 Test2 Natural Integer where
    convertCustomM _ = checkIfPositive . fromIntegral

-- The following also work (requires DeriveAnyClass):
-- > deriving anyclass instance (MonadThrow m) => ConvertCustomM m Test1 Test2 Test1 Test2
-- > deriving anyclass instance (MonadThrow m) => ConvertM m Test1 Test2
deriving via (FromGeneric Test1 Test2) instance (MonadThrow m, forall x y. (Coercible x y => Coercible (m x) (m y))) => ConvertCustomM m Test1 Test2 Test1 Test2
deriving via (FromCustom Test1 Test2) instance (MonadThrow m, forall x y. (Coercible x y => Coercible (m x) (m y))) => ConvertM m Test1 Test2

printSomeException :: SomeException -> IO ()
printSomeException (SomeException e) = print e

unit_convertMFromTest1ToTest2 :: IO ()
unit_convertMFromTest1ToTest2 = handle printSomeException $ print =<< (convertM Test1{a1 = 1, b1 = "test", c1 = True, d1 = 2} :: IO Test2)

unit_convertMFromTest1ToTest2Exception :: IO ()
unit_convertMFromTest1ToTest2Exception = handle printSomeException $ print =<< (convertM Test1{a1 = 0, b1 = "test", c1 = True, d1 = 2} :: IO Test2)