{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeApplications #-}


module THRecords where



import Control.Lens
    ( Prism', preview, view, over, set, Lens', Setter' )         
import Data.Proxy ( Proxy(..) ) 
import Control.Monad.Extra ( concatMapM )
import qualified Data.Text as T
import Data.Char ( isDigit, isLower, toLower )
import Language.Haskell.TH
    ( mkName,
      stringL,
      varP,
      varE,
      conT,
      Exp(LitE, ConE, CaseE, VarE),
      Match(..),
      Clause(Clause),
      Q,
      Pat(ListP, VarP, WildP, LitP),
      Type(ListT, ConT, AppT),
      Dec(NewtypeD, DataD, InstanceD),
      Name,
      DecsQ,
      Con(RecC, NormalC),
      Info(TyConI),
      Body(NormalB),
      nameBase,
      reify )
import Language.Haskell.TH.Syntax ( VarBangType )
import FieldClasses ( StringyLens(..), Primitive )
import THUtils ( (-@>), (@=), isInstanceOf, isSumType )
import Data.Either ( lefts ) 
import THWrappers ( trans, Possibly ) 
import Text.Read (readMaybe)



-----
-- Functions to use in the body of the StringyLens instance declarations. Mainly exist to save me the hassle of doing this all in a TH splice.
------
readInt :: T.Text -> Maybe Int
readInt n = readMaybe (T.unpack n)

proxyOf :: Setter' _a b -> Proxy b
proxyOf _ = Proxy

proxyOfLens :: Lens' a b -> Proxy b
proxyOfLens _ = Proxy

proxyOfPrism :: Prism' a b -> Proxy b
proxyOfPrism _ = Proxy 

proxyOfPrismL :: Prism' a [b] -> Proxy b
proxyOfPrismL _ = Proxy

normalRecUpdate ::  Primitive c 
                => Lens' s c 
                -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)]) 
                -> Either T.Text (s -> [s])
normalRecUpdate optic f = case f (proxyOfLens $ optic ) of
    Right fs -> Right $ \z -> map (\f' -> over optic f' z) fs
    Left str -> Left str

nonBottomRecUpdate :: (StringyLens s, StringyLens c) => [T.Text] -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)] ) -> Lens' s c -> Either T.Text (s -> [s])
nonBottomRecUpdate ys f  optic = case  update (proxyOfLens optic) ys f of
                            Right f' -> Right $ \s -> case view optic s of
                                c -> let cs = f' c
                                     in map (\c' -> set optic c' s) cs
                            Left str -> Left str





applyToPrimNormalRec :: Primitive d => (forall b. Primitive b => Proxy b -> Either T.Text (b ->  c)) -> Lens' a d -> Either T.Text (a -> Maybe c)
applyToPrimNormalRec f optic = case f (proxyOfLens optic) of
    Right f' ->  Right $  \x -> pure f' <*> (preview optic x)
    Left str -> Left str


applyToPrimNonBottom :: (StringyLens d, Monoid c) => [T.Text] -> (forall b. Primitive b => Proxy b -> Either T.Text (b -> c)) -> Lens' t d -> Either T.Text (t -> Maybe c)
applyToPrimNonBottom ys f optic = case applyTo (proxyOfLens optic) ys f of
            Right f' ->  Right $ \y ->  f' (view optic y)
            Left str ->  Left str
 


sumNormalUpdate :: Primitive c =>  (forall b. Primitive b => Proxy b ->  Either T.Text [(b -> b)] ) -> Prism' a c -> Either T.Text (a -> [a])
sumNormalUpdate f myPrism = case (f $ proxyOf myPrism) of
    Right fs  -> Right $ \z -> map (\f' -> over myPrism f' z) fs
    Left  str -> Left str

sumNonBottomUpdate :: (StringyLens a, StringyLens c) =>  [T.Text] -> (forall b. Primitive b => Proxy b ->  Either T.Text [(b ->  b)] ) -> Prism' a c -> Either T.Text (a -> [a])
sumNonBottomUpdate ys f myPrism =  case update (proxyOfPrism myPrism) ys f of
    Right f' -> Right $ \a -> case preview myPrism a of
        Just c -> map (\c' -> set myPrism c' a) (f' c)
        Nothing     -> []
    Left str -> Left str




sumNormalApplyTo :: Primitive c => Prism' a c ->  (forall b. Primitive b => Proxy b -> Either T.Text (b -> d)) -> Either T.Text (a -> Maybe d)
sumNormalApplyTo myPrism f =  (f $ proxyOfPrism myPrism)  >>= \f' -> (Right $ \x -> f' <$>  preview myPrism x)


sumNonBottomApplyTo :: (StringyLens c, Monoid d) => [T.Text] -> Prism' a c ->  (forall b. Primitive b => Proxy b -> Either T.Text (b -> d)) -> Either T.Text (a -> Maybe d)
sumNonBottomApplyTo ys myPrism f = case applyTo (proxyOfPrism myPrism) ys f of
            Right f' ->  Right $ \y -> case preview myPrism y of
                Just t  -> f' t
                Nothing -> Nothing
            Left str    ->  Left str


listUpdate :: forall c d s. (StringyLens s, StringyLens d, c ~ [d]) 
           => Proxy s 
           -> [T.Text] 
           -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)]) 
           -> Lens' s c 
           -> Either T.Text (s -> [s])
listUpdate _ (ys) f optic 
    = let g = update (Proxy @d) ys f
      in case g of
        Left e -> Left e
        Right g' -> Right $ \p ->  
                let dss  = map g' $ view optic p
                in map (\x -> set optic x p) dss

listApplyTo ::  forall a d c e . (StringyLens a, StringyLens d, e ~ [d], Monoid c) => [T.Text] -> (forall b. Primitive b => Proxy b -> Either T.Text (b -> c)) -> Lens' a e -> Either T.Text (a -> Maybe c)
listApplyTo ys f optic
    = case applyTo (Proxy @d) ys f of
        Left e -> Left e
        Right r -> Right $ \p -> case preview optic p of
            Just q -> mapM (r) q >>= \x -> pure $ mconcat x  
            Nothing -> Nothing 



transformerUpdate :: forall s t c. (StringyLens s, StringyLens c, Possibly t c) 
                 => Proxy t 
                 -> [T.Text] 
                 -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)]) 
                 -> Lens' s c
                 -> Either T.Text (s -> [s])
transformerUpdate prox ys f optic = case nonBottomRecUpdate ys f optic of
                 Left err -> Left err 
                 Right f -> Right $ \d -> f $ trans optic prox d 


mkTransformerMatch :: Name -> Name -> Type -> Exp  -> Q Match
mkTransformerMatch sumConNm lensNm typ f = do
    let ysNm = mkName "ys"
    let opticE = VarE $ nameToLensName lensNm 
    let opticStrLTrans = LitP . stringL 
                $ ((formatLens lensNm) <> "@" <> nameBase sumConNm)
    myPat <- [p| $(return opticStrLTrans) : $(varP ysNm) |]
    myProxy <- [| (Proxy :: Proxy $(return typ)) |]
    myBody <- [| transformerUpdate $(return myProxy) $(varE ysNm) $(return f) $(return opticE) |]
    return $ Match myPat (NormalB myBody) []


proxyA :: forall a b. Lens' a b -> Proxy a
proxyA _ = Proxy 


deduplicate :: (Foldable t, Eq a) => t a -> [a]
deduplicate xs = foldr (\x y -> x : filter (/= x) y) [] xs



formatLens :: Name -> String
formatLens  nm =  case dropWhile (\x -> isLower x || isDigit x || x == '_') . nameBase $ nm of
    (x : xs) -> toLower x : xs
    ys       -> ys

nameToLensName :: Name -> Name
nameToLensName = mkName . dropWhile (== '_') . nameBase

nameToPrismName :: Name -> Name
nameToPrismName = mkName . ('_' :) . nameBase

isPrimitive :: Name -> Q Bool
isPrimitive nm = do isInstanceOf nm ''Primitive 

deriveStringyLens :: Name -> DecsQ
deriveStringyLens nm = do
    isPrim <- isPrimitive nm
    if isPrim
        then return []
        else do
            isSum <- isSumType nm
            TyConI dec <- reify nm
            case dec of
                DataD _c _n _bnd _k dCons _ -> if isSum then goSum _n dCons else goProd _n dCons
                NewtypeD _c _n _bnd _k dCon _ -> goNewtype _n dCon

                _ -> fail $ "Error: " <> nameBase nm <> " is not a normal data declaration or newtype. Cannot derive an instance of class ApplyToPrim"
 where

     goNewtype :: Name -> Con -> DecsQ
     goNewtype nm c = do
         case c of
             NormalC _ _ -> fail $ "At the moment, only newtypes with record constructors (and lenses) are supported. " <> nameBase nm <> " appears to be a newtype with a normal constructor, so StringyLens cannot be derived."
             RecC nm' vbts -> do
                 areAllPrims <- allPrims vbts
                 case areAllPrims of
                     Just vbts' -> do
                         myApplyToMatches <- makeNewtypeApplyToMatch vbts
                         myUpdateMatches  <- makeNewtypeUpdateMatch nm vbts 
                         f <- varE . mkName $ "f"

                         let myApplyToFun = mkName "applyTo" @=  [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs") $ myApplyToMatches <> [emptyApplyToMatch])  ) [] ]

                         let myUpdateFun = mkName "update" @= [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $ myUpdateMatches <> [emptyUpdateMatch])  ) [] ]

                         return $ [InstanceD Nothing [] (AppT (ConT . mkName $ "StringyLens") (ConT nm)) [myUpdateFun,myApplyToFun]] 
                     Nothing -> do
                         case getVBTTypeNames vbts of
                             Just ns -> do

                                 children <- concatMapM deriveStringyLens $ deduplicate ns

                                 myUpdateMatches <- makeNewtypeUpdateMatch nm vbts 
                                 myApplyToMatches <- makeNewtypeApplyToMatch vbts
                                 f <- varE . mkName $ "f"

                                 let myApplyToFun = mkName "applyTo" @=  [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs") $ myApplyToMatches <> [emptyApplyToMatch])  ) [] ]

                                 let myUpdateFun = mkName "update" @= [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $ myUpdateMatches <> [emptyUpdateMatch])  ) [] ]

                                 let parent = [InstanceD Nothing [] (AppT (ConT . mkName $ "StringyLens") (ConT nm)) [myUpdateFun,myApplyToFun]] 

                                 return . deduplicate $ children <> parent 


                        

     goProd :: Name -> [Con] -> DecsQ
     goProd n cs = do
        let myVBTs = getRecVBTs cs
        f <- varE . mkName $  "f"
        areAllPrims <- allPrims myVBTs
        case areAllPrims of
            Just vbts' ->  do
                myApplyToMatches <- makeRecCApplyToMatch myVBTs
                myUpdateMatches <- makeRecCUpdateMatch n vbts'
                f <- varE . mkName $ "f"

                let myApplyToFun = mkName "applyTo" @=  [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs") $ myApplyToMatches <> [emptyApplyToMatch])  ) [] ]

                let myUpdateFun = mkName "update" @= [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $ myUpdateMatches <> [emptyUpdateMatch])  ) [] ]

                return $ [InstanceD Nothing [] (AppT (ConT . mkName $ "StringyLens") (ConT n)) [myUpdateFun,myApplyToFun]]
            Nothing -> do
                case getVBTTypeNames myVBTs of
                    Just ns -> do
                        sumProds <- sortSumProd ns

                        let sumChildrenNames = lefts sumProds


                        children <- concatMapM deriveStringyLens $ deduplicate ns
                        myUpdateMatches <- makeRecCUpdateMatch n myVBTs
                        myApplyToMatches <- makeRecCApplyToMatch myVBTs
                        f <- varE . mkName $ "f"

                        let myApplyToFun = mkName "applyTo" @=  [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $ myApplyToMatches <> [emptyApplyToMatch])  ) [] ]

                        let myUpdateFun = mkName "update" @= [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $ myUpdateMatches <> [emptyUpdateMatch])  ) [] ]


                        let parent = [InstanceD Nothing [] (AppT (ConT . mkName $ "StringyLens") (ConT n)) [myUpdateFun,myApplyToFun]]
                        return $ deduplicate $ children <> parent
                    Nothing -> fail $ "Unsupported type in data type: " <> nameBase n

     goSum :: Name -> [Con] -> DecsQ
     goSum n cs = do
         let namesBTs = map ((\(x,y) -> (x,map snd y))
                          .  (\(NormalC conName bts) -> (conName,bts))) cs
         let checkedForUnary = sequence $ foldr (\(x,ys) z -> case ys of
                [t] -> Just (x,t) : z
                _   -> Nothing : z) [] namesBTs
         case checkedForUnary of
             Just ts -> do
                 let typNames = mapM (\case
                        (x,ConT n)               -> Just n
                        (x, AppT ListT (ConT n)) -> Just n
                        _                        -> Nothing ) ts
                 case typNames of
                     Just ns -> do
                         allPrimCons <- allPrimNames ns
                         if allPrimCons
                             then do
                                myUpdateMatches <- makeSumUpdateMatches n ts
                                myApplyToMatches <- makeSumApplyToMatches ts

                                let myApplyToFun = mkName "applyTo" @=  [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $  myApplyToMatches <> [emptyApplyToMatch])  ) [] ]

                                let myUpdateFun = mkName "update" @= [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $ myUpdateMatches <> [emptyUpdateMatch])  ) [] ]

                                return $ [InstanceD Nothing [] (AppT (ConT . mkName $ "StringyLens") (ConT n)) [myUpdateFun,myApplyToFun]]

                             else do
                                children <- concatMapM deriveStringyLens $ deduplicate ns

                                myUpdateMatches <- makeSumUpdateMatches n ts
                                myApplyToMatches <- makeSumApplyToMatches ts

                                let myApplyToFun = mkName "applyTo" @=  [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs") $  myApplyToMatches <> [emptyApplyToMatch])  ) [] ]

                                let myUpdateFun = mkName "update" @= [Clause [WildP, (VarP . mkName $ "strs"), (VarP . mkName $ "f")] (NormalB $ (CaseE (VarE . mkName $ "strs")  $ myUpdateMatches <> [emptyUpdateMatch])  ) [] ]

                                let parent = [InstanceD Nothing [] (AppT (ConT . mkName $ "StringyLens") (ConT n)) [myUpdateFun,myApplyToFun]]
                                return $ deduplicate $ children <> parent
                     Nothing -> return []
             Nothing -> fail $ "Unsupported sum type: " <> nameBase n <> ". Only sum types with single argument constructors are supported."




     --goNewtype :: Name -> Con -> DecsQ
     --goNewtype n cs = undefined

type SumType = Name
type ProductType = Name 

sortSumProd :: [Name] -> Q [Either SumType ProductType]
sortSumProd ns = mapM go ns
    where
        go :: Name -> Q  (Either SumType ProductType)
        go n = do
            isSum <- isSumType n
            if isSum then return $ Left n else return $ Right n

getSumTypeConNames :: Name -> Q [(Name,Type)]
getSumTypeConNames nm = do
    TyConI (DataD _ _ _ _ constructors _ ) <- reify nm
    let conNames = foldr (\(NormalC cnNm [(b,t)]) acc -> (cnNm,t) : acc) [] constructors
    return conNames 


allPrimNames :: [Name] -> Q (Bool)
allPrimNames ns = and <$> mapM isPrimitive ns


getVBTTypeNames :: [VarBangType] -> Maybe [Name]
getVBTTypeNames vbts =
    mapM (\case
        (v,b, ConT nm)       -> Just nm
        (v,b, AppT ListT (ConT nm)) -> Just nm
        _                    -> Nothing ) vbts

getListTypesVBT :: [VarBangType] -> Maybe [Type]
getListTypesVBT vbts =
    mapM (\case
        (v,b,AppT ListT (ConT nm)) -> Just (AppT ListT (ConT nm))
        _                          -> Nothing) vbts


getRecVBTs :: [Con] -> [VarBangType]
getRecVBTs cs = concatMap (\case
    RecC n vbts ->  vbts
    _           -> []) cs


makeSumUpdateMatches :: Name -> [(Name,Type)] -> Q [Match]
makeSumUpdateMatches parentType nts = do
    f <- varE . mkName $  "f"
    mapM (go f) nts
   where
       go :: Exp -> (Name,Type) -> Q Match
       go f (n,t) = do
            case t of
                (ConT name) -> do
                    isPrim <- isPrimitive name
                    if isPrim
                        then mkSumNormalUpdateMatch n f
                        else mkSumNonBottomUpdateMatch n f
                (AppT ListT _) -> do
                     mkSumNonBottomUpdateMatch n f -- mkSumListUpdateMatch n f
                _ -> fail $ "Unsupported type"


makeSumApplyToMatches :: [(Name,Type)] -> Q [Match]
makeSumApplyToMatches  nts = do
    f <- varE . mkName $  "f"
    mapM (go f) nts
   where
       go :: Exp -> (Name,Type) -> Q Match
       go f (n,t) = do
            case t of
                (ConT name) -> do
                    isPrim <- isPrimitive name
                    if isPrim
                        then mkSumNormalApplyToMatch n f
                        else mkSumNonBottomApplyToMatch n f
                (AppT ListT _) -> do
                     mkSumNonBottomApplyToMatch n f --mkSumListApplyToMatch n f
                _ -> fail $ "Unsupported type"

makeRecCUpdateMatch :: Name -> [VarBangType] -> Q [Match]
makeRecCUpdateMatch parentType vbts = do
    f <- varE . mkName $  "f"
    concat <$> mapM (go f) vbts
   where
        go :: Exp -> VarBangType -> Q [Match]
        go  f (nm,_,t) = do
            case t of
                (ConT name) -> do
                    isPrim <- isPrimitive name
                    isSum  <- isSumType name 
                    if | isPrim -> do 
                           m1 <- mkUpdatePrimBottomRecMatch nm f
                           return [m1]
                       | isSum  -> do
                           m1 <- mkUpdatePrimNonBottomRecMatch nm f
                           m2 <- getSumTypeConNames name >>= \x -> mapM (\(sNm,sT) -> mkTransformerMatch sNm nm sT f) x
                           return $ [m1] <> m2 
                       | otherwise -> do 
                           m1 <- mkUpdatePrimNonBottomRecMatch nm f
                           return [m1]
                            
                (AppT ListT name) -> do
                     m1 <- mkListUpdateMatch parentType nm f-- mkUpdatePrimNonBottomRecMatch nm f --mkUpdatePrimListMatch nm f
                     return [m1]
                _ -> fail $ "Unsupported type"

makeNewtypeUpdateMatch :: Name -> [VarBangType] -> Q [Match]
makeNewtypeUpdateMatch parentType vbts = do
    f <- varE . mkName $  "f"
    concat <$> mapM (go f) vbts
   where
        go :: Exp -> VarBangType -> Q [Match]
        go  f (nm,_,t) = do
            case t of
                (ConT name) -> do
                    isPrim <- isPrimitive name
                    isSum  <- isSumType name 
                    if | isPrim -> do 
                           m1 <- mkNewtypeBottomNormalMatch nm f
                           return [m1]

                       | otherwise -> do 
                           m1 <- mkUpdatePrimNonBottomRecMatch nm f
                           return [m1]
                            
                (AppT ListT name) -> do
                     m1 <- mkListUpdateMatch parentType nm f-- mkUpdatePrimNonBottomRecMatch nm f --mkUpdatePrimListMatch nm f
                     return [m1]
                _ -> fail $ "Unsupported type"


makeRecCApplyToMatch :: [VarBangType] -> Q [Match]
makeRecCApplyToMatch vbts = do
    f <- varE . mkName $ "f"
    mapM (go f) vbts
   where
       go :: Exp -> VarBangType -> Q Match
       go f (nm,_,t) = do
            case t of
                (ConT name) -> do
                    isPrim <- isPrimitive name
                    if isPrim
                        then mkApplyToPrimNormalRec nm f
                        else mkApplyToPrimNonBottom nm f
                (AppT ListT _) -> do
                     mkListApplyToMatch nm f-- mkApplyToPrimNonBottom nm f --mkApplyToPrimList nm f
                _ -> fail $ "Unsupported type"

makeNewtypeApplyToMatch :: [VarBangType] -> Q [Match]
makeNewtypeApplyToMatch vbts = do
    f <- varE . mkName $ "f"
    mapM (go f) vbts
   where
       go :: Exp -> VarBangType -> Q Match
       go f (nm,_,t) = do
            case t of
                (ConT name) -> do
                    isPrim <- isPrimitive name
                    if isPrim
                        then mkNewtypeApplyToNormalMatch    nm f
                        else mkNewtypeApplyToNonBottomMatch nm f
                (AppT ListT _) -> do
                     mkListApplyToMatch nm f-- mkApplyToPrimNonBottom nm f --mkApplyToPrimList nm f
                _ -> fail $ "Unsupported type"

allPrims :: [VarBangType] -> Q (Maybe [VarBangType])
allPrims vbts = do
    mVBTs <- mapM go vbts
    return $ sequence mVBTs
    where
        go :: VarBangType -> Q (Maybe VarBangType)
        go (v,b,t) = do
            case t of
                ConT name -> do
                    isPrim <- isPrimitive name
                    if isPrim then return $ Just (v,b,t) else return Nothing
                AppT ListT (ConT name) -> do
                    isPrim <- isPrimitive name
                    if isPrim then return $ Just (v,b,t) else return Nothing
                _ -> fail $ "Error: " <> nameBase v <> " is a record that contains an unsupported type."


emptyUpdateMatch :: Match
emptyUpdateMatch = Match WildP (NormalB $ (ConE . mkName $ "Left") -@> (LitE . stringL $ "Error: Invalid record selector!")) []

emptyApplyToMatch :: Match
emptyApplyToMatch = Match WildP (NormalB $ (ConE . mkName $ "Left") -@> (LitE . stringL $ "Error: Invalid record selector!")) []

mkListUpdateMatch :: Name -> Name -> Exp -> Q Match
mkListUpdateMatch parentType nm f = do
    let ysNm = mkName "ys"
    let opticE = VarE $ nameToLensName nm
    let opticStrL = LitP . stringL $ formatLens nm
    let proxA = [| proxyA $(return opticE) |]
    myBody <- [| (listUpdate $(proxA) $(varE ysNm) $(return f) $(return opticE) :: Either T.Text ( $(conT parentType) -> [ $(conT parentType) ] )) |]
    myPat  <- [p| $(return opticStrL) : $(varP ysNm) |]
    return $ Match myPat (NormalB $ myBody) []


mkNewtypeBottomNormalMatch :: Name -> Exp -> Q Match
mkNewtypeBottomNormalMatch nm f = do
    let opticE = VarE $ nameToLensName nm
    myPat  <- [p| [] |]
    myBody <- [|normalRecUpdate $(return opticE) $(return f) |]
    return $ Match myPat (NormalB $ myBody) []

mkNewtypeNonBottomUpdateMatch :: Name -> Exp -> Q Match
mkNewtypeNonBottomUpdateMatch nm f = do
    let opticE = VarE $ nameToLensName nm
    let ysNm = mkName "ys"
    myBody <- [| nonBottomRecUpdate $(varE ysNm) $(return f) $(return opticE) |]
    myPat  <- [p| $(varP ysNm) |]
    return $ Match myPat (NormalB myBody) []
    
mkNewtypeApplyToNormalMatch :: Name -> Exp -> Q Match
mkNewtypeApplyToNormalMatch nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE . nameToLensName $ nm
    myPat <- [p| [] |]
    myBody <- [| applyToPrimNormalRec $(return f)  $(return opticE) |]
    return $ Match myPat (NormalB $ myBody) []

mkNewtypeApplyToNonBottomMatch :: Name -> Exp -> Q Match
mkNewtypeApplyToNonBottomMatch nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE . nameToLensName $ nm
    myPat <- [p| $(varP ysNm) |]
    myBody <- [| applyToPrimNonBottom $(varE ysNm) $(return f) $(return opticE) |]
    return $ Match myPat (NormalB $ myBody) []


-- f is the variable representing the 2nd arg to overprim
mkUpdatePrimBottomRecMatch :: Name -> Exp -> Q Match
mkUpdatePrimBottomRecMatch nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE $ nameToLensName nm
    let opticStrL = LitP . stringL $ formatLens nm
    myBody <- [| normalRecUpdate $(return opticE) $(return f) |]
    myPat <-  [p| $(return opticStrL) : $(varP ysNm) |]
    --return $  Match opticP (NormalB $ (mapE -@> ((overE -@> opticE))) -@> (f -@> (proxyOfE -@> opticE))) []
    return $ Match myPat (NormalB $ myBody) []

mkUpdatePrimNonBottomRecMatch :: Name -> Exp -> Q Match
mkUpdatePrimNonBottomRecMatch nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE $ nameToLensName nm
    let opticStrL = LitP . stringL $ formatLens nm
    myPat <- [p| $(return opticStrL) : $(varP ysNm) |]
    myBody <- [| nonBottomRecUpdate $(varE ysNm) $(return f) $(return opticE) |]
    return $ Match myPat (NormalB $ myBody) []


mkListApplyToMatch :: Name -> Exp -> Q Match 
mkListApplyToMatch nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE . nameToLensName $ nm 
    let opticP =  LitP . stringL $ formatLens nm
    myPat <- [p| $(return opticP) : $(varP ysNm) |]
    myBody <- [| listApplyTo $(varE ysNm) $(return f) $(return opticE) |]
    return $ Match myPat (NormalB $ myBody) []


mkApplyToPrimNormalRec :: Name -> Exp -> Q Match
mkApplyToPrimNormalRec nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE . nameToLensName $ nm
    let opticP = ListP . map (LitP . stringL) $ [formatLens nm]
    myBody <- [| applyToPrimNormalRec $(return f)  $(return opticE) |]
    return $ Match opticP (NormalB $ myBody) []

mkApplyToPrimNonBottom :: Name -> Exp -> Q Match
mkApplyToPrimNonBottom nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE . nameToLensName $ nm
    let opticStrL = LitP . stringL $ formatLens nm
    myPat <-  [p| $(return opticStrL) : $(varP ysNm) |]
    myBody <- [| applyToPrimNonBottom $(varE ysNm) $(return f) $(return opticE) |]
    return $ Match myPat (NormalB $ myBody) []



mkSumNormalUpdateMatch :: Name -> Exp -> Q Match
mkSumNormalUpdateMatch nm f = do
    let opticE = VarE $ nameToPrismName nm
    let opticStrL = LitP . stringL $ map toLower . nameBase $  nm
    myBody <- [| sumNormalUpdate $(return f) $(return opticE)  |]
    myPat <-  [p| [ $(return opticStrL) ] |]
    return $ Match myPat (NormalB $ myBody) []

mkSumNonBottomUpdateMatch :: Name -> Exp -> Q Match
mkSumNonBottomUpdateMatch nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE $ nameToPrismName nm
    let opticStrL = LitP . stringL $ map toLower . nameBase $  nm
    myBody <- [| sumNonBottomUpdate $(varE ysNm) $(return f) $(return opticE)  |]
    myPat <-  [p| $(return opticStrL) : $(varP ysNm) |]
    return $ Match myPat (NormalB $ myBody) []


mkSumNormalApplyToMatch :: Name -> Exp -> Q Match
mkSumNormalApplyToMatch nm f = do
    let opticE = VarE $ nameToPrismName nm
    let opticStrL = LitP . stringL $ map toLower . nameBase $  nm
    myBody <- [| sumNormalApplyTo  $(return opticE) $(return f) |]
    myPat <-  [p| [ $(return opticStrL) ] |]
    return $ Match myPat (NormalB $ myBody) []

mkSumNonBottomApplyToMatch :: Name -> Exp -> Q Match
mkSumNonBottomApplyToMatch nm f = do
    let ysNm = mkName $ "ys"
    let opticE = VarE $ nameToPrismName nm
    let opticStrL = LitP . stringL $ map toLower . nameBase $  nm
    myBody <- [| sumNonBottomApplyTo $(varE ysNm) $(return opticE) $(return f) |]
    myPat <-  [p| $(return opticStrL) : $(varP ysNm) |]
    return $ Match myPat (NormalB $ myBody) []


