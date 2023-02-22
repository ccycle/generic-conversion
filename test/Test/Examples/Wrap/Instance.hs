-- Tips to avoid orphan instances for `Convert` class
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Examples.Wrap.Instance where

import Data.Coerce
import Data.Generic.Conversion
import Data.Text
import GHC.Generics
import qualified Test.Examples.Wrap.ModuleA as A
import qualified Test.Examples.Wrap.ModuleB as B

newtype Wrap a = Wrap a deriving newtype (Generic)
unWrap :: Wrap a -> a
unWrap = coerce

instance ConvertCustom A.MyType (Wrap B.MyType) Integer Int where
    convertCustom _ = fromInteger
instance ConvertCustom A.MyType (Wrap B.MyType) String Text where
    convertCustom _ = pack

deriving via (FromGeneric A.MyType B.MyType) instance ConvertCustom A.MyType (Wrap B.MyType) A.MyType (Wrap B.MyType)
deriving via (FromCustom A.MyType (Wrap B.MyType)) instance Convert A.MyType (Wrap B.MyType)

unit_convertMyTypeA1 :: IO ()
unit_convertMyTypeA1 = print (unWrap . convert $ A.MyType{a = 1, b = "text"} :: B.MyType)

-- deriving via (FromGeneric A.MyType B.MyType) instance Convert A.MyType (Wrap B.MyType)
