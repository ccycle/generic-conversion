{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Generic.Conversion.TH (
    deriveConvert,
    deriveConvertFromAnyclass,
    deriveConvertWithCheckingConNamesOrder,
    GDatatype (..),
    GConNames (..),
) where

import Control.Applicative (liftA2)
import Data.Foldable (foldl')
import Data.Generic.Conversion
import qualified Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Language.Haskell.TH

newtype ExpressionException = ExpressionException Exp deriving stock (Show)

displayExpressionExceptionQ :: ExpressionException -> Q String
displayExpressionExceptionQ (ExpressionException astExp) = do
    i <- runQ (pure astExp)
    pure $ unwords ["ExpressionException: Input expression", "`" <> pprint i <> "`", "does not match expected expression such as `f :: Type -> Type`."]

(<<>>) :: Q [Dec] -> Q [Dec] -> Q [Dec]
(<<>>) = liftA2 (<>)

declareInstanceConvertCustom ::
    Cxt ->
    FromDataType ->
    ToDataType ->
    Type ->
    Type ->
    Exp ->
    Dec
declareInstanceConvertCustom context (FromDataType fromDataType) (ToDataType toDataType) fromType toType func =
    InstanceD
        Nothing
        context
        (AppT (AppT (AppT (AppT (ConT ''ConvertCustom) fromDataType) toDataType) fromType) toType)
        [FunD 'convertCustom [Clause [WildP] (NormalB func) []]]

data DerivingStrategyOption = DerivingViaOpt | DeriveAnyClassOpt

deriveStrategyFromOpt ::
    Name ->
    DerivingStrategyOption ->
    FromDataType ->
    ToDataType ->
    Maybe DerivStrategy
deriveStrategyFromOpt viaTypeName DerivingViaOpt (FromDataType{..}) (ToDataType{..}) = Just (ViaStrategy (AppT (AppT (ConT viaTypeName) fromDataType) toDataType))
deriveStrategyFromOpt _ DeriveAnyClassOpt _ _ = Just AnyclassStrategy

standaloneDeriveConvert :: DerivingStrategyOption -> FromDataType -> ToDataType -> [Dec]
standaloneDeriveConvert opt f@FromDataType{..} t@ToDataType{..} =
    [ StandaloneDerivD (deriveStrategyFromOpt ''FromGeneric opt f t) [] (AppT (AppT (AppT (AppT (ConT ''ConvertCustom) fromDataType) toDataType) fromDataType) toDataType)
    , StandaloneDerivD (deriveStrategyFromOpt ''FromCustom opt f t) [] (AppT (AppT (ConT ''Convert) fromDataType) toDataType)
    ]

data ExtractFuncInfo = ExtractFuncInfo
    { func :: Exp
    , tyVarBndrs :: [TyVarBndr Specificity]
    , context :: Cxt
    , fromType :: Type
    , toType :: Type
    }
    deriving stock (Show)

extractFuncInfo :: Q Exp -> Q ExtractFuncInfo
extractFuncInfo astExpQ = do
    astExp <- astExpQ
    case astExp of
        SigE func (AppT (AppT ArrowT fromType) toType) -> pure ExtractFuncInfo{context = [], tyVarBndrs = [], ..}
        SigE func (ForallT tyVarBndrs context (AppT (AppT ArrowT fromType) toType)) -> pure ExtractFuncInfo{..}
        a -> error <$> displayExpressionExceptionQ (ExpressionException a)

extractFuncInfoList :: [Q Exp] -> Q [ExtractFuncInfo]
extractFuncInfoList = traverse extractFuncInfo

newtype FromDataType = FromDataType {fromDataType :: Type}
newtype ToDataType = ToDataType {toDataType :: Type}

deriveInstanceConvertCustomFromExtractedInfo :: FromDataType -> ToDataType -> ExtractFuncInfo -> Dec
deriveInstanceConvertCustomFromExtractedInfo fromDataType toDataType ExtractFuncInfo{..} =
    declareInstanceConvertCustom context fromDataType toDataType fromType toType func

deriveConvertCustoms :: FromDataType -> ToDataType -> [Q Exp] -> Q [Dec]
deriveConvertCustoms fromDataType toDataType expQs = do
    extractFuncInfoQs <- extractFuncInfoList expQs
    pure $ fmap (deriveInstanceConvertCustomFromExtractedInfo fromDataType toDataType) extractFuncInfoQs

deriveConvertWithOpt :: DerivingStrategyOption -> Name -> Name -> [Q Exp] -> Q [Dec]
deriveConvertWithOpt opt name1 name2 expQs = deriveConvertCustoms fromData toData expQs <<>> pure (standaloneDeriveConvert opt fromData toData)
  where
    fromData = FromDataType (ConT name1)
    toData = ToDataType (ConT name2)

deriveConvert :: Name -> Name -> [Q Exp] -> Q [Dec]
deriveConvert = deriveConvertWithOpt DerivingViaOpt

deriveConvertFromAnyclass :: Name -> Name -> [Q Exp] -> Q [Dec]
deriveConvertFromAnyclass = deriveConvertWithOpt DeriveAnyClassOpt

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

checkConNamesOrder :: forall a b. (GConNames (Rep a), GConNames (Rep b)) => Proxy a -> Proxy b -> Bool
checkConNamesOrder proxy1 proxy2 = eqOrders (conNamesProxy proxy1) (conNamesProxy proxy2)

type ConNamesOrderErrorMessage a b =
    'Text "The orders of constructor names do not match: " ':<>: 'ShowType a ':<>: 'Text ", " ':<>: 'ShowType b

deriveConvertWithCheckingConNamesOrder ::
    ( GDatatype (Rep a1)
    , GDatatype (Rep a2)
    , GConNames (Rep a1)
    , GConNames (Rep a2)
    ) =>
    Proxy a1 ->
    Proxy a2 ->
    Q [Dec]
deriveConvertWithCheckingConNamesOrder proxyA proxyB = do
    let aType = ConT $ mkName $ datatypeNameProxy proxyA
        bType = ConT $ mkName $ datatypeNameProxy proxyB
    if checkConNamesOrder proxyA proxyB
        then pure [StandaloneDerivD (deriveStrategyFromOpt ''FromGeneric DerivingViaOpt (FromDataType aType) (ToDataType bType)) [] (AppT (AppT (ConT ''Convert) aType) bType)]
        else pure [InstanceD Nothing [] (AppT (AppT (ConT ''Convert) aType) bType) [ValD (VarP 'convert) (NormalB (SigE (AppE (VarE 'error) (LitE (StringL "unreachable"))) (ForallT [] [AppT (ConT ''TypeError) (AppT (AppT (ConT ''ConNamesOrderErrorMessage) aType) bType)] (AppT (AppT ArrowT aType) bType)))) []]]
