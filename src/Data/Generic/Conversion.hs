{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Generic.Conversion where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.Natural (Natural)

class Convert a b where
    convert :: a -> b
    default convert :: (ConvertCustom a b a b) => a -> b
    convert = (convertCustom :: ConvertCustom a b a b => Proxy2 a b -> a -> b) Proxy2

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

-- Generic
newtype FromGeneric a b = FromGeneric b
instance (Generic a, Generic b, GConvert (Rep a) (Rep b)) => Convert a (FromGeneric a b) where
    convert = FromGeneric . to . gconvert . from

-- Functor
newtype FromFunctor f a b = FromFunctor (f b)
instance (Functor f, Convert a b) => Convert (f a) (FromFunctor f a b) where
    convert = FromFunctor . fmap convert
deriving via (FromFunctor [] a b) instance (Convert a b) => Convert [a] [b]
deriving via (FromFunctor Maybe a b) instance (Convert a b) => Convert (Maybe a) (Maybe b)
deriving via (FromFunctor (Either e) a b) instance (Convert a b) => Convert (Either e a) (Either e b)

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

----------------------------------------------------------------

-- Custom convert
data Proxy2 d1 d2 = Proxy2

class ConvertCustom d1 d2 a b where
    convertCustom :: Proxy2 d1 d2 -> a -> b
    default convertCustom :: (Generic a, Generic b, GConvertCustom d1 d2 (Rep a) (Rep b)) => Proxy2 d1 d2 -> a -> b
    convertCustom p = to . gconvertCustom p . from

instance (Generic a, Generic b, GConvertCustom d1 d2 (Rep a) (Rep b)) => ConvertCustom d1 d2 a (FromGeneric a b) where
    convertCustom p = (FromGeneric . to) . gconvertCustom p . from

newtype FromCustom a b = FromCustom b
instance (ConvertCustom a b a b) => Convert a (FromCustom a b) where
    convert = FromCustom . (convertCustom :: ConvertCustom a b a b => Proxy2 a b -> a -> b) Proxy2

instance ConvertCustom d1 d2 a a where
    convertCustom _ = id

instance (Functor f, ConvertCustom d1 d2 a b) => ConvertCustom d1 d2 (f a) (FromFunctor f a b) where
    convertCustom proxy = FromFunctor . fmap (convertCustom proxy)
deriving via (FromFunctor [] a b) instance (ConvertCustom d1 d2 a b) => ConvertCustom d1 d2 [a] [b]
deriving via (FromFunctor Maybe a b) instance (ConvertCustom d1 d2 a b) => ConvertCustom d1 d2 (Maybe a) (Maybe b)
deriving via (FromFunctor (Either e) a b) instance (ConvertCustom d1 d2 a b) => ConvertCustom d1 d2 (Either e a) (Either e b)

class GConvertCustom d1 d2 f g where
    gconvertCustom :: Proxy2 d1 d2 -> f a -> g a

instance GConvertCustom d1 d2 U1 U1 where
    gconvertCustom _ = id

instance (GConvertCustom d1 d2 f1 g1, GConvertCustom d1 d2 f2 g2) => GConvertCustom d1 d2 (f1 :*: f2) (g1 :*: g2) where
    gconvertCustom p (f :*: g) = gconvertCustom p f :*: gconvertCustom p g

instance (GConvertCustom d1 d2 f1 g1, GConvertCustom d1 d2 f2 g2) => GConvertCustom d1 d2 (f1 :+: f2) (g1 :+: g2) where
    gconvertCustom p (L1 a) = L1 $ gconvertCustom p a
    gconvertCustom p (R1 b) = R1 $ gconvertCustom p b

instance (ConvertCustom d1 d2 a b) => GConvertCustom d1 d2 (K1 i1 a) (K1 i2 b) where
    gconvertCustom p = K1 . convertCustom p . unK1

instance (GConvertCustom d1 d2 f g) => GConvertCustom d1 d2 (M1 i1 t1 f) (M1 i2 t2 g) where
    gconvertCustom p = M1 . gconvertCustom p . unM1
