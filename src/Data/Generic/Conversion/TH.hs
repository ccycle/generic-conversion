{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Generic.Conversion.TH (deriveConvert) where

import Control.Applicative (liftA2)
import Data.Generic.Conversion
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
    ConvertFromDataType ->
    ConvertToDataType ->
    Type ->
    Type ->
    Exp ->
    Dec
declareInstanceConvertCustom context (ConvertFromDataType fromDataType) (ConvertToDataType toDataType) fromType toType func =
    InstanceD
        Nothing
        context
        (AppT (AppT (AppT (AppT (ConT ''ConvertCustom) fromDataType) toDataType) fromType) toType)
        [FunD 'convertCustom [Clause [WildP] (NormalB func) []]]

declareConvertAnyclass :: ConvertFromDataType -> ConvertToDataType -> [Dec]
declareConvertAnyclass (ConvertFromDataType fromDataType) (ConvertToDataType toDataType) =
    [ StandaloneDerivD (Just AnyclassStrategy) [] (AppT (AppT (AppT (AppT (ConT ''ConvertCustom) fromDataType) toDataType) fromDataType) toDataType)
    , StandaloneDerivD (Just AnyclassStrategy) [] (AppT (AppT (ConT ''Convert) fromDataType) toDataType)
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

newtype ConvertFromDataType = ConvertFromDataType Type
newtype ConvertToDataType = ConvertToDataType Type

deriveInstanceConvertCustomFromExtractedInfo :: ConvertFromDataType -> ConvertToDataType -> ExtractFuncInfo -> Dec
deriveInstanceConvertCustomFromExtractedInfo fromDataType toDataType ExtractFuncInfo{..} =
    declareInstanceConvertCustom context fromDataType toDataType fromType toType func

deriveConvert' :: ConvertFromDataType -> ConvertToDataType -> [Q Exp] -> Q [Dec]
deriveConvert' fromDataType toDataType expQs = do
    extractFuncInfoQs <- extractFuncInfoList expQs
    pure $ fmap (deriveInstanceConvertCustomFromExtractedInfo fromDataType toDataType) extractFuncInfoQs

deriveConvert :: Name -> Name -> [Q Exp] -> Q [Dec]
deriveConvert name1 name2 expQs = deriveConvert' fromData toData expQs <<>> pure (declareConvertAnyclass fromData toData)
  where
    fromData = ConvertFromDataType (ConT name1)
    toData = ConvertToDataType (ConT name2)