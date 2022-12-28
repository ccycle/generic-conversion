{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Conversion.Generic.TH (deriveConvert, deriveBidirectionalConvert) where

import Control.Applicative (liftA2)
import Data.Coerce (coerce)
import Data.Conversion.Generic (Convert (..))
import GHC.Generics
import Language.Haskell.TH

newtype ExpressionException = ExpressionException Exp deriving stock (Show)

displayExpressionExceptionQ :: ExpressionException -> Q String
displayExpressionExceptionQ (ExpressionException astExp) = do
    i <- runQ (pure astExp)
    pure $ unwords ["ExpressionException: Input expression", "`" <> pprint i <> "`", "does not match expected expression such as `f :: Type -> Type`."]

data ExtractFuncInfo = ExtractFuncInfo {func :: Exp, typeFrom :: Type, typeTo :: Type} deriving stock (Show)

extractFuncInfo :: Q Exp -> Q ExtractFuncInfo
extractFuncInfo astExpQ = do
    astExp <- astExpQ
    case astExp of
        SigE f (AppT (AppT ArrowT x) y) -> pure ExtractFuncInfo{func = f, typeFrom = x, typeTo = y}
        a -> error <$> displayExpressionExceptionQ (ExpressionException a)

newtype ConvertFromType = ConvertFromType {convertFromType :: Name}
newtype ConvertToType = ConvertToType {convertToType :: Name}

newtype ConvertClassName = ConvertClassName {convertClassName :: Name}
newtype ConvertMethodName = ConvertMethodName {convertMethodName :: Name}
newtype GConvertClassName = GConvertClassName {gconvertClassName :: Name}
newtype GConvertMethodName = GConvertMethodName {gconvertMethodName :: Name}

newtype NamePrefix = NamePrefix String

addPrefix :: NamePrefix -> Name -> Name
addPrefix (NamePrefix p) n = mkName $ p <> nameBase n

addPrefixGenericsClass :: ConvertClassName -> GConvertClassName
addPrefixGenericsClass (ConvertClassName n) = GConvertClassName (addPrefix (NamePrefix "G") n)
addPrefixGenericsMethod :: ConvertMethodName -> GConvertMethodName
addPrefixGenericsMethod (ConvertMethodName n) = GConvertMethodName (addPrefix (NamePrefix "g") n)

generateConvertMethodName :: ConvertFromType -> ConvertToType -> ConvertMethodName
generateConvertMethodName (ConvertFromType n1) (ConvertToType n2) = ConvertMethodName $ mkName $ nameBase 'convert <> "From" <> nameBase n1 <> "To" <> nameBase n2
generateConvertClassName :: ConvertFromType -> ConvertToType -> ConvertClassName
generateConvertClassName (ConvertFromType n1) (ConvertToType n2) = ConvertClassName $ mkName $ nameBase ''Convert <> "From" <> nameBase n1 <> "To" <> nameBase n2

declareClasses :: ConvertClassName -> ConvertMethodName -> Q [Dec]
declareClasses convertClassNameVar@ConvertClassName{..} convertMethodNameVar@ConvertMethodName{..} = do
    a <- newName "a"
    b <- newName "b"
    f <- newName "f"
    g <- newName "g"
    f1 <- newName "f1"
    g1 <- newName "g1"
    f2 <- newName "f2"
    g2 <- newName "g2"
    i1 <- newName "i1"
    i2 <- newName "i2"
    t1 <- newName "t1"
    t2 <- newName "t2"
    let GConvertClassName{..} = addPrefixGenericsClass convertClassNameVar
        GConvertMethodName{..} = addPrefixGenericsMethod convertMethodNameVar
    pure
        [ ClassD [] convertClassName [plainTV a, plainTV b] [] [SigD convertMethodName (AppT (AppT ArrowT (VarT a)) (VarT b)), DefaultSigD convertMethodName (ForallT [] [AppT (ConT ''Generic) (VarT a), AppT (ConT ''Generic) (VarT b), AppT (AppT (ConT gconvertClassName) (AppT (ConT ''Rep) (VarT a))) (AppT (ConT ''Rep) (VarT b))] (AppT (AppT ArrowT (VarT a)) (VarT b))), ValD (VarP convertMethodName) (NormalB (InfixE (Just (VarE 'to)) (VarE '(.)) (Just (InfixE (Just (VarE gconvertMethodName)) (VarE '(.)) (Just (VarE 'from)))))) []]
        , InstanceD Nothing [] (AppT (AppT (ConT convertClassName) (VarT a)) (VarT a)) [ValD (VarP convertMethodName) (NormalB (VarE 'id)) []]
        , ClassD [] gconvertClassName [plainTV f, plainTV g] [] [SigD gconvertMethodName (AppT (AppT ArrowT (AppT (VarT f) (VarT a))) (AppT (VarT g) (VarT a)))]
        , InstanceD Nothing [] (AppT (AppT (ConT gconvertClassName) (ConT ''U1)) (ConT ''U1)) [ValD (VarP gconvertMethodName) (NormalB (VarE 'id)) []]
        , InstanceD Nothing [AppT (AppT (ConT gconvertClassName) (VarT f)) (VarT g)] (AppT (AppT (ConT gconvertClassName) (AppT (AppT (AppT (ConT ''M1) (VarT i1)) (VarT t1)) (VarT f))) (AppT (AppT (AppT (ConT ''M1) (VarT i2)) (VarT t2)) (VarT g))) [ValD (VarP gconvertMethodName) (NormalB (InfixE (Just (ConE 'M1)) (VarE '(.)) (Just (InfixE (Just (VarE gconvertMethodName)) (VarE '(.)) (Just (VarE 'unM1)))))) []]
        , InstanceD Nothing [AppT (AppT (ConT convertClassName) (VarT a)) (VarT b)] (AppT (AppT (ConT gconvertClassName) (AppT (AppT (ConT ''K1) (VarT i1)) (VarT a))) (AppT (AppT (ConT ''K1) (VarT i2)) (VarT b))) [ValD (VarP gconvertMethodName) (NormalB (InfixE (Just (ConE 'K1)) (VarE '(.)) (Just (InfixE (Just (VarE convertMethodName)) (VarE '(.)) (Just (VarE 'unK1)))))) []]
        , InstanceD Nothing [AppT (AppT (ConT gconvertClassName) (VarT f1)) (VarT g1), AppT (AppT (ConT gconvertClassName) (VarT f2)) (VarT g2)] (AppT (AppT (ConT gconvertClassName) (AppT (AppT (ConT ''(:+:)) (VarT f1)) (VarT f2))) (AppT (AppT (ConT ''(:+:)) (VarT g1)) (VarT g2))) [FunD gconvertMethodName [Clause [conPLocal 'L1 [VarP a]] (NormalB (InfixE (Just (ConE 'L1)) (VarE '($)) (Just (AppE (VarE gconvertMethodName) (VarE a))))) [], Clause [conPLocal 'R1 [VarP b]] (NormalB (InfixE (Just (ConE 'R1)) (VarE '($)) (Just (AppE (VarE gconvertMethodName) (VarE b))))) []]]
        , InstanceD Nothing [AppT (AppT (ConT gconvertClassName) (VarT f1)) (VarT g1), AppT (AppT (ConT gconvertClassName) (VarT f2)) (VarT g2)] (AppT (AppT (ConT gconvertClassName) (AppT (AppT (ConT ''(:*:)) (VarT f1)) (VarT f2))) (AppT (AppT (ConT ''(:*:)) (VarT g1)) (VarT g2))) [FunD gconvertMethodName [Clause [InfixP (VarP f) '(:*:) (VarP g)] (NormalB (InfixE (Just (AppE (VarE gconvertMethodName) (VarE f))) (ConE '(:*:)) (Just (AppE (VarE gconvertMethodName) (VarE g))))) []]]
        ]
    where
        conPLocal :: Name -> [Pat] -> Pat
        conPLocal n pats =
#if MIN_VERSION_template_haskell(2,18,0)
            ConP n [] pats
#else
            ConP n pats
#endif

declareConvertClassAndInstance :: ConvertFromType -> ConvertToType -> Q [Dec]
declareConvertClassAndInstance t1 t2 = declareClasses c m
  where
    c = generateConvertClassName t1 t2
    m = generateConvertMethodName t1 t2

-- [[|someFunc1 :: T1 -> T2|]
--
-- instance ConvertAutogen T1 T2 where
--     convertAutogen = someFunc
declareInstanceConvertFromFuncInfo :: ConvertClassName -> ConvertMethodName -> ExtractFuncInfo -> Dec
declareInstanceConvertFromFuncInfo ConvertClassName{..} ConvertMethodName{..} ExtractFuncInfo{..} =
    InstanceD Nothing [] (AppT (AppT (ConT convertClassName) typeFrom) typeTo) [ValD (VarP convertMethodName) (NormalB func) []]

declareInstanceConvertFromExpQ :: ConvertClassName -> ConvertMethodName -> Q Exp -> Q Dec
declareInstanceConvertFromExpQ c m expQ = fmap (declareInstanceConvertFromFuncInfo c m) (extractFuncInfo expQ)

-- [[|someFunc1 :: T1 -> T2|], [|someFunc2 :: T3 -> T4|]]
--
-- instance ConvertAutogen T1 T2 where
--     convertAutogen = someFunc1
-- instance ConvertAutogen T3 T4 where
--     convertAutogen = someFunc2
declareInstanceConvertFromExpQs :: ConvertClassName -> ConvertMethodName -> [Q Exp] -> Q [Dec]
declareInstanceConvertFromExpQs c m = traverse (declareInstanceConvertFromExpQ c m)

-- deriving anyclass instance ConvertFromDataType1ToDataType2 Test1 Test2
declareStandaloneInstanceConvert :: Applicative f => ConvertFromType -> ConvertToType -> ConvertClassName -> f [Dec]
declareStandaloneInstanceConvert ConvertFromType{..} ConvertToType{..} ConvertClassName{..} =
    pure [StandaloneDerivD (Just AnyclassStrategy) [] (AppT (AppT (ConT convertClassName) (ConT convertFromType)) (ConT convertToType))]

-- instance Convert DataType1 DataType2 where
--     convert = convertFromDataType1ToDataType2
declareInstanceConvert :: Applicative f => ConvertFromType -> ConvertToType -> ConvertMethodName -> f [Dec]
declareInstanceConvert ConvertFromType{..} ConvertToType{..} ConvertMethodName{..} =
    pure [InstanceD Nothing [] (AppT (AppT (ConT ''Convert) (ConT convertFromType)) (ConT convertToType)) [ValD (VarP 'convert) (NormalB (VarE convertMethodName)) []]]

(<<>>) :: Q [Dec] -> Q [Dec] -> Q [Dec]
(<<>>) = liftA2 (<>)

deriveConvert :: Name -> Name -> [Q Exp] -> Q [Dec]
deriveConvert n1 n2 expQs =
    let t1 = ConvertFromType n1
        t2 = ConvertToType n2
        c = generateConvertClassName t1 t2
        m = generateConvertMethodName t1 t2
     in declareConvertClassAndInstance t1 t2 <<>> declareInstanceConvertFromExpQs c m expQs <<>> declareStandaloneInstanceConvert t1 t2 c <<>> declareInstanceConvert t1 t2 m

deriveBidirectionalConvert :: Name -> Name -> [Q Exp] -> Q [Dec]
deriveBidirectionalConvert n1 n2 expQs =
    let t1 = ConvertFromType n1
        t2 = ConvertToType n2
        c = generateConvertClassName t1 t2
        m = generateConvertMethodName t1 t2
     in deriveConvert n1 n2 expQs <<>> declareStandaloneInstanceConvert (coerce t2) (coerce t1) c <<>> declareInstanceConvert (coerce t2) (coerce t1) m
