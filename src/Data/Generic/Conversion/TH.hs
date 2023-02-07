{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Generic.Conversion.TH (
    deriveConvert,
    deriveConvertFromAnyclass,
    testConNamesOrderTH,
) where

import Control.Applicative (liftA2)
import Data.Generic.Conversion
import Data.Generic.Conversion.Internal
import Data.List (intercalate)
import Data.Proxy
import GHC.Generics
import GHC.Stack
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

testConNamesOrderTH :: forall a b. (HasCallStack, GDatatype (Rep a), GConNames (Rep a), GDatatype (Rep b), GConNames (Rep b)) => Proxy a -> Proxy b -> Q [Dec]
testConNamesOrderTH proxy1 proxy2 =
    if testConNamesOrder proxy1 proxy2
        then runIO (putStrLn msgOK) >> pure []
        else withFrozenCallStack error msg
  where
    dName1 = datatypeNameProxy (Proxy :: Proxy a)
    dName2 = datatypeNameProxy (Proxy :: Proxy b)
    msgOK = "[TH] Check orders for the constructor names in " ++ dName1 ++ " and " ++ dName2 ++ "; OK"
    msg =
        unwords
            [ "[TH]"
            , "The orders of constructor names in "
            , dName1
            , "and"
            , dName2
            , "do not match;"
            , "\n"
            , "\n"
            , dName1 ++ ":"
            , "\n"
            , indent (conNamesProxy proxy1)
            , "\n"
            , dName2 ++ ":"
            , "\n"
            , indent (conNamesProxy proxy2)
            , "\n"
            ]
    indent = intercalate "\n" . map ('\t' :)