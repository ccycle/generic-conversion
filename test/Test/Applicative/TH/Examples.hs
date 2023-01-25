{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Applicative.TH.Examples where

import Control.Exception.Safe
import Data.Generic.Conversion.Applicative
import Data.Generic.Conversion.Applicative.TH
import Data.Text
import GHC.Generics
import GHC.Natural

data Test1 = Test1 {a1 :: Int, b1 :: String, c1 :: Bool, d1 :: Natural} deriving stock (Show, Generic)
data Test2 = Test2 {a2 :: Integer, b2 :: Text, c2 :: Bool, d2 :: Integer} deriving stock (Show, Generic)

data NegativeException = NegativeException deriving stock (Eq, Show)
instance Exception NegativeException

checkIfPositive :: (Ord a, Num a, MonadThrow f) => a -> f a
checkIfPositive a = if a >= 0 then pure a else throwM NegativeException

-- `deriveConvertMFromAnyclass` also work (requires `DeriveAnyClass` instead of `DerivingVia`)
deriveConvertM
    ''Test1
    ''Test2
    [ [|checkIfPositive . fromIntegral :: (MonadThrow m, Integral a) => a -> m Integer|]
    , [|pure . pack :: MonadThrow m => String -> m Text|]
    , [|pure :: MonadThrow m => Bool -> m Bool|]
    ]

printSomeException :: SomeException -> IO ()
printSomeException (SomeException e) = print e

unit_convertMFromTest1ToTest2TH :: IO ()
unit_convertMFromTest1ToTest2TH = handle printSomeException $ print =<< (convertM Test1{a1 = 1, b1 = "test", c1 = True, d1 = 2} :: IO Test2)

unit_convertMFromTest1ToTest2ExceptionTH :: IO ()
unit_convertMFromTest1ToTest2ExceptionTH = handle printSomeException $ print =<< (convertM Test1{a1 = 0, b1 = "test", c1 = True, d1 = 2} :: IO Test2)