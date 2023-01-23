{-# LANGUAGE UndecidableInstances #-}

module Data.Generic.Conversion.Applicative (
    module Data.Generic.Conversion.Applicative,
    module Data.Generic.Conversion,
)
where

import Control.Applicative (liftA2)
import Data.Generic.Conversion (FromCustom (..), FromGeneric (..), Proxy2 (..))
import GHC.Generics

class ConvertM m a b where
    convertM :: a -> m b
    default convertM :: (ConvertCustomM m a b a b) => a -> m b
    convertM = (convertCustomM :: ConvertCustomM m a b a b => Proxy2 a b -> a -> m b) (Proxy2 :: Proxy2 a b)

instance (ConvertCustomM m a b a b) => ConvertM m a (FromCustom a b) where
    convertM = fmap FromCustom . (convertCustomM :: ConvertCustomM m a b a b => Proxy2 a b -> a -> m b) (Proxy2 :: Proxy2 a b)

class Applicative m => ConvertCustomM m d1 d2 a b where
    convertCustomM :: Proxy2 d1 d2 -> a -> m b
    default convertCustomM :: (Generic a, Generic b, GConvertCustomM m d1 d2 (Rep a) (Rep b)) => Proxy2 d1 d2 -> a -> m b
    convertCustomM p = fmap to . gconvertCustomM p . from

instance (Applicative m, Generic a, Generic b, GConvertCustomM m d1 d2 (Rep a) (Rep b)) => ConvertCustomM m d1 d2 a (FromGeneric a b) where
    convertCustomM p = fmap (FromGeneric . to) . gconvertCustomM p . from

class GConvertCustomM m d1 d2 f g where
    gconvertCustomM :: Proxy2 d1 d2 -> f a -> m (g a)

instance (Applicative m, GConvertCustomM m d1 d2 f1 g1, GConvertCustomM m d1 d2 f2 g2) => GConvertCustomM m d1 d2 (f1 :*: f2) (g1 :*: g2) where
    gconvertCustomM p (f :*: g) = liftA2 (:*:) (gconvertCustomM p f) (gconvertCustomM p g)

instance (Applicative m, GConvertCustomM m d1 d2 f1 g1, GConvertCustomM m d1 d2 f2 g2) => GConvertCustomM m d1 d2 (f1 :+: f2) (g1 :+: g2) where
    gconvertCustomM p (L1 a) = L1 <$> gconvertCustomM p a
    gconvertCustomM p (R1 b) = R1 <$> gconvertCustomM p b

instance (ConvertCustomM m d1 d2 a b) => GConvertCustomM m d1 d2 (K1 i1 a) (K1 i2 b) where
    gconvertCustomM p = fmap K1 . convertCustomM p . unK1

instance (Applicative m, GConvertCustomM m d1 d2 f g) => GConvertCustomM m d1 d2 (M1 i1 t1 f) (M1 i2 t2 g) where
    gconvertCustomM p = fmap M1 . gconvertCustomM p . unM1
