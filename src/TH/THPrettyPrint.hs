{-# LANGUAGE TemplateHaskell, OverloadedStrings #-} 


module THPrettyPrint where

import Language.Haskell.TH hiding (pprint)
import Classes
import Control.Lens 
import FieldClasses 
import GPrettyPrint 
import PrimTypes
import qualified Data.Text as T 
import THUtils
import THRecords
import Data.Either 
import Control.Monad
import Control.Monad.Extra (concatMapM)


---                  ---  
--- WORK IN PROGRESS ---
---                  ---

instance PrettyPrint a => PrettyPrint [a] where
    pprint m x = T.concat $ map (pprint m) x

type IsTopLevel = Bool 

ppDeriver :: DecsQ
ppDeriver = do
    myProtocols <- getAllProtocols
    decs <- concatMapM (derivePrettyPrint True) myProtocols 
    decs' <-  dedupInstances decs
    return decs'
derivePrettyPrint :: IsTopLevel -> Name -> DecsQ
derivePrettyPrint bewl nm = do
    info <- reify nm
    isPretty <- isInstanceOf nm ''PrettyPrint
    if isPretty then return [] else do
        case info of
            TyConI dec -> do  
                case dec of
                    (DataD a _ c d cons f ) -> do  
                            isSum <- isSumType nm
                            if isSum
                                then  goSum bewl nm cons 
                                else goProd bewl nm cons 
                    (NewtypeD a _ c d con f) -> goNewtype bewl nm con
                    _      -> do 
                        reportWarning $ "\n\n\n\n\n\n Error deriving prettyprint for type  " <> (nameBase nm) <> "\n\n\n\n"  
                        return []
            _ -> do 
                reportWarning $ "\n\n\n\n\n\n Error deriving prettyprint for type  " <> (nameBase nm) <> "\n\n\n\n"  
                return []
   where

       goSum :: IsTopLevel -> Name -> [Con] -> DecsQ
       goSum b n cs = do
           let myVTs = concatMap (\(NormalC prismNm bts) -> map (\x -> (prismNm,snd x)) bts) cs 
           myVTs' <- filterPretty $ deduplicate . getTypeNamesVT $ myVTs 
           children <- concat <$>  (mapM (derivePrettyPrint False) $ myVTs')
           let typeString = LitE . stringL . nameBase $ nm
           let varXP = (VarP . mkName $ "x")
           let varMP = (VarP . mkName $ "m")
           matches <- mapM (mkSumMatch (VarE . mkName $ "m")) $ map fst myVTs
           let myPPFun = mkPPrintSum matches 
           let parent = 
                  [InstanceD Nothing [] (AppT (ConT . mkName $ "PrettyPrint") (ConT n)) [myPPFun]]
           return $  (children <> parent)

       goNewtype :: IsTopLevel -> Name -> Con -> DecsQ
       goNewtype b newtypeName c = do
           case c of
               RecC recNm [(lenNm,_,(ConT childTypNm))] -> do
                    children <- derivePrettyPrint False $ childTypNm 
                    body <- [| gPrettyPrint' $(litE . stringL . nameBase $ newtypeName) $(varE . mkName $ "m") $(varE . mkName $ "x") |]
                    parent <- [d|
                        instance PrettyPrint $(conT newtypeName) where
                            pprint $(varP . mkName $ "m") $(varP . mkName $ "x") = $(return body)
                                    |]
                    return $  children <> parent

               _ -> fail "Impossible newtype?" 

       goProd :: IsTopLevel -> Name -> [Con] -> DecsQ
       goProd b n cs = do
           let myVBTs = concatMap (\(RecC recNm vbts) -> vbts) cs
           let myVTs  = map (\(v,b,t) -> (v,t)) myVBTs 
           allPrims <- and <$> mapM (\(v,t) -> case t of
                        ConT n' -> isInstanceOf n' ''Primitive
                        _      -> return False) myVTs
           if allPrims
               then let nmTxt = LitE . stringL $ nameBase n
                    in if b

                        then [d| 
                        instance PrettyPrint $(conT nm) where
                            pprint m x = gPrettyPrint (T.pack $(return nmTxt) ) m x 
                               |]

                        else [d| 
                        instance PrettyPrint $(conT nm) where
                            pprint m x = gPrettyPrint' (T.pack $(return nmTxt) ) m x 
                               |]
              else do
                  children <- mapM (derivePrettyPrint False) $ getTypeNamesVT myVTs
                  sorted <- mapM sortRecords myVTs
                  let grouped = groupRecords sorted
                  rows <-  mapM (mkRow False) grouped 
                  body <-  mapM mkBody rows
                  let body' = ListE body 
                  let typeString = LitE . stringL . nameBase $ n
                  let varXP = (VarP . mkName $ "x")
                  let varMP = (VarP . mkName $ "m")
                  isTop <- if b then [| True |] else [| False |]
                  parent  <- [d|
                    instance PrettyPrint $(conT n) where
                        pprint $(return varMP) $(return varXP) = makeLabelRow (T.pack $(return typeString)) <> foldr (<>) dashRow $(return body') 
                                |]
                  return $ (concat children <> parent)

getInstanceType :: Dec -> Type
getInstanceType (InstanceD _ _ typ _) = typ

dedupInstances :: [Dec] -> Q [Dec]
dedupInstances 
    = foldr (\x acc -> case x of
        InstanceD _ _ typ _ -> pure (x :)  <*> (filter (\z -> getInstanceType z /= typ) <$> acc)
        _ -> acc) (pure []) 



getTypeName :: (Name,Type) -> Maybe Name
getTypeName (_,t) = case t of 
    ConT n -> Just n
    AppT ListT (ConT n) -> Just n
    _ -> Nothing

getTypeNamesVT :: [(Name,Type)] -> [Name]
getTypeNamesVT ns = case mapM getTypeName ns of
    Just x -> x 
    Nothing -> []

mkPPrintSum :: [Match] -> Dec
mkPPrintSum ms = 
    mkName "pprint" @= [Clause [(VarP . mkName $ "m"), (VarP . mkName $ "x")] (NormalB $ (CaseE (VarE . mkName $ "x") $ ms)) []]


mkSumMatch :: Exp ->  Name -> Q Match 
mkSumMatch varM sumCon = do
    let myPat = ConP sumCon [ (VarP $  mkName "y")]
    myBody <- [| pprint $(return varM)  $(varE . mkName $ "y") |]
    return $ Match myPat (NormalB $ myBody) []
  

mkBody :: Either Exp [Exp] -> Q Exp
mkBody x = case x of
    Right xs -> do
        let xs' = return $  ListE xs
        [| makeDataRow $(xs') |]
    Left x -> [| $(return x) |]

filterPretty :: [Name] -> Q [Name]
filterPretty ns = concat <$> mapM go ns
    where
        go :: Name -> Q [Name]
        go n = do
            isPretty <- isInstanceOf n ''PrettyPrint
            if isPretty then return [] else return [n]

type IsSum = Bool 

mkRow :: IsSum ->  Either Name [Name] -> Q (Either Exp [Exp])
mkRow b x = 
    case x of
        Left n -> do
            let lensNm = VarE $ if b then nameToPrismName n else nameToLensName n
            let varX = (VarE . mkName $ "x")
            let varM = (VarE . mkName $ "m")
            Left <$> [| pprint $(return varM) $ view $(return lensNm) $(return varX) |]
        Right nts -> do
            let lensStrings = map (\a -> LitE . stringL $ formatLens a) nts 
            let lensified = map (\a -> (VarE $ nameToLensName a)) nts
            let varX = (VarE . mkName $ "x")
            let varM = (VarE . mkName $ "m")
            Right <$> zipWithM (go varX varM) lensStrings lensified
   where
       go :: Exp -> Exp -> Exp -> Exp -> Q Exp
       go varx varm lstr lensE = [| (T.pack $(return lstr)) <> ": " <> (pprint $(return varm) $ $(return varx) ^. $(return lensE))  |]



groupRecords :: [Either a b] -> [Either a [b]]
groupRecords [] = []
groupRecords xs = go xs []
    where
        go :: [Either a b] -> [Either a [b]] -> [Either a [b]]
        go [] acc = acc
        go (x:xs) acc = case x of
            Right x' -> let morePrims = foldr (\y z -> if isRight y then pure (:) <*> y <*> z else pure []) (Right []) xs
                            dropPrims = dropWhile isRight xs
                       in go dropPrims ( acc  <> [pure (:) <*> x <*> morePrims]) 
            Left x'  -> go xs (acc <> [Left x'])

 
sortRecords :: (Name,Type) -> Q (Either (Name) (Name))
sortRecords (n,t) = do
    case t of
        (ConT x) -> do
            isPrim <- isInstanceOf x ''Primitive
            if isPrim 
                then return $ Right n
                else return $ Left  n
        _ -> return $ Left n
