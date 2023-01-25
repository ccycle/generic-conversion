-- Tips to avoid orphan instances for `Convert` class
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tips.Convert where

import Data.Coerce
import Data.Generic.Conversion
import GHC.Generics
import qualified Test.Tips.ModuleA as A
import qualified Test.Tips.ModuleB as B

newtype Wrap a = Wrap a deriving newtype (Generic)
unWrap :: Wrap a -> a
unWrap = coerce

deriving via (FromGeneric A.MyType B.MyType) instance Convert A.MyType (Wrap B.MyType)

unit_convertMyTypeA1 :: IO ()
unit_convertMyTypeA1 = print (unWrap . convert $ A.MyType1 :: B.MyType)
unit_convertMyTypeA2 :: IO ()
unit_convertMyTypeA2 = print (unWrap . convert $ A.MyType2 :: B.MyType)
