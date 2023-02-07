{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Generic.Conversion.Internal (
    GConNames (..),
    GDatatype (..),
    conNamesProxy,
    datatypeNameProxy,
    testConNamesOrder,
) where

import Data.Foldable (foldl')
import qualified Data.Kind
import Data.Proxy
import GHC.Generics

class GConNames (f :: Data.Kind.Type -> Data.Kind.Type) where
    gconNames :: Proxy f -> [String]
instance (GConNames f) => GConNames (M1 D t f) where
    gconNames _ = gconNames (Proxy :: Proxy f)
instance (GConNames f, GConNames g) => GConNames (f :+: g) where
    gconNames _ = gconNames (Proxy :: Proxy f) ++ gconNames (Proxy :: Proxy g)
instance (Constructor c) => GConNames (C1 c f) where
    gconNames _ = [conName (undefined :: C1 c f g)]

conNamesProxy :: forall t. (GConNames (Rep t)) => Proxy t -> [String]
conNamesProxy _ = gconNames (Proxy :: Proxy (Rep t))

datatypeNameProxy ::
    forall a.
    (GDatatype (Rep a)) =>
    Proxy a ->
    String
datatypeNameProxy _ = gdatatypeName (Proxy :: Proxy (Rep a))

class GDatatype (f :: Data.Kind.Type -> Data.Kind.Type) where
    gdatatypeName :: Proxy f -> String
instance (Datatype t) => GDatatype (M1 d t f) where
    gdatatypeName _ = datatypeName (undefined :: M1 d t f a)

ordersBool :: Ord a => [a] -> [Bool]
ordersBool [] = []
ordersBool (x : xs) = fst $ foldl' step ([], x) xs
  where
    step (b, val1) val2 = (b ++ [val1 <= val2], val2)

eqList :: Eq a => [a] -> [a] -> Bool
eqList l1 l2 = go l1 l2 True
  where
    go (x : xs) (y : ys) b = go xs ys (b && x == y)
    go [] [] True = True
    go _ _ _ = False

eqOrders :: (Ord a1, Ord a2) => [a1] -> [a2] -> Bool
eqOrders l1 l2 = eqList (ordersBool l1) (ordersBool l2)

testConNamesOrder :: forall a b. (GConNames (Rep a), GConNames (Rep b)) => Proxy a -> Proxy b -> Bool
testConNamesOrder proxy1 proxy2 = eqOrders (conNamesProxy proxy1) (conNamesProxy proxy2)
