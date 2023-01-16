{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Conversion.Generic where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Conversion.Generic.Custom
import Data.Int
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import GHC.Generics
import GHC.Natural (Natural)

class Convert a b where
    convert :: a -> b
    default convert :: (Generic a, Generic b, GConvert (Rep a) (Rep b)) => a -> b
    convert = to . gconvert . from

instance Convert a a where
    convert = id
    {-# INLINE convert #-}

-- Generics
class GConvert f g where
    gconvert :: f a -> g a

instance GConvert U1 U1 where
    gconvert = id

instance (GConvert f1 g1, GConvert f2 g2) => GConvert (f1 :*: f2) (g1 :*: g2) where
    gconvert (f :*: g) = gconvert f :*: gconvert g

instance (GConvert f1 g1, GConvert f2 g2) => GConvert (f1 :+: f2) (g1 :+: g2) where
    gconvert (L1 a) = L1 $ gconvert a
    gconvert (R1 b) = R1 $ gconvert b

instance (Convert a b) => GConvert (K1 i1 a) (K1 i2 b) where
    gconvert = K1 . convert . unK1

instance (GConvert f g) => GConvert (M1 i1 t1 f) (M1 i2 t2 g) where
    gconvert = M1 . gconvert . unM1

-- ConvertCustom
newtype ConvertCustomType a = ConvertCustomType a
instance (ConvertCustom a b a b) => Convert a (ConvertCustomType b) where
    convert = ConvertCustomType . (convertCustom :: ConvertCustom a b a b => Proxy2 a b -> a -> b) Proxy2

-- Integral
newtype IntegralType a = IntegralType a
instance (Integral a, Num b) => Convert a (IntegralType b) where
    convert = IntegralType . fromIntegral
    {-# INLINE convert #-}

deriving via (IntegralType Integer) instance Convert Int8 Integer
deriving via (IntegralType Integer) instance Convert Int16 Integer
deriving via (IntegralType Integer) instance Convert Int32 Integer
deriving via (IntegralType Integer) instance Convert Int64 Integer
deriving via (IntegralType Integer) instance Convert Int Integer
deriving via (IntegralType Integer) instance Convert Word8 Integer
deriving via (IntegralType Integer) instance Convert Word16 Integer
deriving via (IntegralType Integer) instance Convert Word32 Integer
deriving via (IntegralType Integer) instance Convert Word64 Integer
deriving via (IntegralType Integer) instance Convert Word Integer
deriving via (IntegralType Integer) instance Convert Natural Integer

-- Int(2^n) (0 <= n <= N) -> Int(2^(N+1))
-- Word(2^n) (0 <= n <= N) -> Int(2^(N+1))
deriving via (IntegralType Int) instance Convert Int8 Int
deriving via (IntegralType Int) instance Convert Int16 Int
deriving via (IntegralType Int) instance Convert Int32 Int
deriving via (IntegralType Int) instance Convert Int64 Int
deriving via (IntegralType Int) instance Convert Word8 Int
deriving via (IntegralType Int) instance Convert Word16 Int
deriving via (IntegralType Int) instance Convert Word32 Int

deriving via (IntegralType Int16) instance Convert Int8 Int16
deriving via (IntegralType Int16) instance Convert Word8 Int16

deriving via (IntegralType Int32) instance Convert Int8 Int32
deriving via (IntegralType Int32) instance Convert Int16 Int32
deriving via (IntegralType Int32) instance Convert Word8 Int32
deriving via (IntegralType Int32) instance Convert Word16 Int32

deriving via (IntegralType Int64) instance Convert Int8 Int64
deriving via (IntegralType Int64) instance Convert Int16 Int64
deriving via (IntegralType Int64) instance Convert Int32 Int64
deriving via (IntegralType Int64) instance Convert Word8 Int64
deriving via (IntegralType Int64) instance Convert Word16 Int64
deriving via (IntegralType Int64) instance Convert Word32 Int64

-- Word(2^n) (0 <= n <= N) -> Word(2^(N+1))
deriving via (IntegralType Word) instance Convert Word8 Word
deriving via (IntegralType Word) instance Convert Word16 Word
deriving via (IntegralType Word) instance Convert Word32 Word
deriving via (IntegralType Word) instance Convert Word64 Word

deriving via (IntegralType Word16) instance Convert Word8 Word16

deriving via (IntegralType Word32) instance Convert Word16 Word32
deriving via (IntegralType Word32) instance Convert Word32 Word32

deriving via (IntegralType Word64) instance Convert Word8 Word64
deriving via (IntegralType Word64) instance Convert Word16 Word64
deriving via (IntegralType Word64) instance Convert Word32 Word64
deriving via (IntegralType Word64) instance Convert Word Word64

-- Word(2^n) -> Natural
deriving via (IntegralType Natural) instance Convert Word8 Natural
deriving via (IntegralType Natural) instance Convert Word16 Natural
deriving via (IntegralType Natural) instance Convert Word32 Natural
deriving via (IntegralType Natural) instance Convert Word64 Natural
deriving via (IntegralType Natural) instance Convert Word Natural

-- IsString
newtype IsStringType a = IsStringType a
instance (IsString a) => Convert String (IsStringType a) where
    convert = IsStringType . fromString
    {-# INLINE convert #-}

deriving via (IsStringType T.Text) instance Convert String T.Text
deriving via (IsStringType TL.Text) instance Convert String TL.Text
deriving via (IsStringType B.ByteString) instance Convert String B.ByteString
deriving via (IsStringType BL.ByteString) instance Convert String BL.ByteString
deriving via (IsStringType BB.Builder) instance Convert String BB.Builder
deriving via (IsStringType BS.ShortByteString) instance Convert String BS.ShortByteString
