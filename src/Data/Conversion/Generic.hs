module Data.Conversion.Generic where

import GHC.Generics

class Convert a b where
    convert :: a -> b
    default convert :: (Generic a, Generic b, GConvert (Rep a) (Rep b)) => a -> b
    convert = to . gconvert . from

instance Convert a a where
    convert = id

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
