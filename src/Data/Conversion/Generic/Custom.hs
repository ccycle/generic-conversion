module Data.Conversion.Generic.Custom where

import GHC.Generics

data Proxy2 d1 d2 = Proxy2

class ConvertCustom d1 d2 a b where
    convertCustom :: Proxy2 d1 d2 -> a -> b
    default convertCustom :: (Generic a, Generic b, GConvertCustom d1 d2 (Rep a) (Rep b)) => Proxy2 d1 d2 -> a -> b
    convertCustom p = to . gconvertCustom p . from

instance ConvertCustom d1 d2 a a where
    convertCustom _ = id

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
