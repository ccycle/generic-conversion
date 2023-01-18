{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Generic.Conversion.Applicative.TH (deriveConvertM) where

import Control.Applicative (liftA2)
import Data.Containers.ListUtils (nubOrd)
import Data.Generic.Conversion.Applicative
import Language.Haskell.TH

newtype ConvertContext = ConvertContext Cxt deriving newtype (Eq, Ord)
newtype ConvertContextConcat = ConvertContextConcat Cxt deriving newtype (Show)
newtype ConvertMonadVar = ConvertMonadVar Type
newtype ConvertFromDataType = ConvertFromDataType Type
newtype ConvertToDataType = ConvertToDataType Type

-- Exception
newtype ExpressionException = ExpressionException Exp deriving stock (Show)

displayExpressionExceptionQ :: ExpressionException -> Q String
displayExpressionExceptionQ (ExpressionException astExp) = do
    i <- runQ (pure astExp)
    pure $ unwords ["ExpressionException: Input expression", "`" <> pprint i <> "`", "does not match expected expression such as `f :: Type -> m Type`."]

(<<>>) :: Q [Dec] -> Q [Dec] -> Q [Dec]
(<<>>) = liftA2 (<>)

-- Declare instance for ConvertCustomM for each `Q Exp`
declareInstanceConvertCustomM ::
    ConvertContext ->
    ConvertMonadVar ->
    ConvertFromDataType ->
    ConvertToDataType ->
    Type ->
    Type ->
    Exp ->
    Dec
declareInstanceConvertCustomM (ConvertContext context) (ConvertMonadVar monad) (ConvertFromDataType fromDataType) (ConvertToDataType toDataType) fromType toType func =
    InstanceD
        Nothing
        context
        (AppT (AppT (AppT (AppT (AppT (ConT ''ConvertCustomM) monad) fromDataType) toDataType) fromType) toType)
        [FunD 'convertCustomM [Clause [WildP] (NormalB func) []]]

-- >>> t = AppT (ConT ''Applicative) (VarT (mkName "m_0"))
-- >>> m0 = mkName "m_0"
-- >>> doesExistVarName m0 t
-- True
doesExistVarName :: Name -> Type -> Bool
doesExistVarName typeVar = go
  where
    go (AppT x y) = go x || go y
    go (VarT n) = n == typeVar
    go _ = False

renameTypes :: (Name -> Name) -> Type -> Type
renameTypes filterFunc = \case
    AppT x y -> AppT (renameTypes filterFunc x) (renameTypes filterFunc y)
    VarT x -> VarT (filterFunc x)
    x -> x

declareConvertCustomM :: ConvertContextConcat -> ConvertMonadVar -> ConvertFromDataType -> ConvertToDataType -> [Dec]
declareConvertCustomM (ConvertContextConcat context) (ConvertMonadVar monad) (ConvertFromDataType fromDataType) (ConvertToDataType toDataType) =
    [ StandaloneDerivD (Just AnyclassStrategy) context (AppT (AppT (AppT (AppT (AppT (ConT ''ConvertCustomM) monad) fromDataType) toDataType) fromDataType) toDataType)
    , StandaloneDerivD (Just AnyclassStrategy) context (AppT (AppT (AppT (ConT ''ConvertM) monad) fromDataType) toDataType)
    ]

data ExtractFuncInfo = ExtractFuncInfo
    { func :: Exp
    , tyVarBndrs :: [TyVarBndr Specificity]
    , context :: Cxt
    , monad :: Type
    , fromType :: Type
    , toType :: Type
    }
    deriving stock (Show)

extractFuncInfo :: Q Exp -> Q ExtractFuncInfo
extractFuncInfo astExpQ = do
    astExp <- astExpQ
    case astExp of
        SigE func (ForallT tyVarBndrs context (AppT (AppT ArrowT fromType) (AppT monad toType))) -> pure ExtractFuncInfo{..}
        a -> error <$> displayExpressionExceptionQ (ExpressionException a)

data CxtInfo = CxtInfo {monad :: Type, context :: Cxt} deriving stock (Show)

filterContext :: ExtractFuncInfo -> CxtInfo
filterContext ExtractFuncInfo{..} = case monad of
    VarT m -> CxtInfo{context = filter (doesExistVarName m) context, ..}
    _ -> CxtInfo{context = [], ..}

replaceCxt :: Name -> CxtInfo -> [Type]
replaceCxt varName CxtInfo{..} = case monad of
    VarT m -> fmap (renameTypes (\x -> if m == x then varName else x)) context
    _ -> context

filterAndConcat :: Foldable t => Name -> t ExtractFuncInfo -> ConvertContextConcat
filterAndConcat varName = ConvertContextConcat . nubOrd . concatMap (replaceCxt varName . filterContext)

extractFuncInfoList :: [Q Exp] -> Q [ExtractFuncInfo]
extractFuncInfoList = traverse extractFuncInfo

filterAndConcatQ :: Name -> [Q Exp] -> Q ConvertContextConcat
filterAndConcatQ varName = fmap (filterAndConcat varName) . extractFuncInfoList

deriveInstanceConvertCustomFromExtractedInfo :: ConvertFromDataType -> ConvertToDataType -> ExtractFuncInfo -> Dec
deriveInstanceConvertCustomFromExtractedInfo fromDataType toDataType ExtractFuncInfo{..} =
    declareInstanceConvertCustomM (ConvertContext context) (ConvertMonadVar monad) fromDataType toDataType fromType toType func

deriveConvertM' :: ConvertFromDataType -> ConvertToDataType -> [Q Exp] -> Q [Dec]
deriveConvertM' fromDataType toDataType expQs = do
    extractFuncInfoQs <- extractFuncInfoList expQs
    pure $ fmap (deriveInstanceConvertCustomFromExtractedInfo fromDataType toDataType) extractFuncInfoQs

deriveConvertM :: Name -> Name -> [Q Exp] -> Q [Dec]
deriveConvertM name1 name2 expQs = do
    m <- newName "m"
    cxtConcat <- filterAndConcatQ m expQs
    deriveConvertM' fromData toData expQs <<>> pure (declareConvertCustomM cxtConcat (ConvertMonadVar (VarT m)) fromData toData)
  where
    fromData = ConvertFromDataType (ConT name1)
    toData = ConvertToDataType (ConT name2)
