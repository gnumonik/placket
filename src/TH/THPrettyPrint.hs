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

type IsTopLevel = Bool 

ppDeriver :: DecsQ
ppDeriver = do
    myProtocols <- getAllProtocols
    concatMapM (derivePrettyPrint True) myProtocols 

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
           children <- concat <$>  (mapM (derivePrettyPrint False) $ getTypeNamesVT myVTs)
           body <- mapM sortRecords myVTs >>= (pure . groupRecords) >>= mapM (mkRow True) >>= mapM mkBody >>= (pure . ListE)
           let typeString = LitE . stringL . nameBase $ nm
           let varX = (VarE . mkName $ "x")
           let varM = (VarE . mkName $ "m")
           parent <- [d|
                    instance PrettyPrint $(conT nm) where
                        pprint m x = makeLabelRow (T.pack $(return typeString)) $ foldr (<>) dashRow $(return body) 
                                |]
           return $ (children <> parent)

       goNewtype :: IsTopLevel -> Name -> Con -> DecsQ
       goNewtype b n c = do
           case c of
               RecC nm vbts -> do
                   let myVTs = map (\(v,b,t) -> (v,t)) vbts
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
                           children <- concat <$>  (mapM (derivePrettyPrint False) $ getTypeNamesVT myVTs)
                           body <- mapM sortRecords myVTs >>= (pure . groupRecords) >>= mapM (mkRow False) >>= mapM mkBody >>= (pure . ListE)
                           let typeString = LitE . stringL . nameBase $ nm
                           parent <- [d|
                            instance PrettyPrint $(conT nm) where
                                pprint m x = makeLabelRow (T.pack $(return typeString)) $ foldr (<>) dashRow $(return body) 
                                        |]
                           return $ (children <> parent)

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
                  children <- concat <$> mapM (derivePrettyPrint False) (getTypeNamesVT myVTs)
                  sorted <- mapM sortRecords myVTs
                  let grouped = groupRecords sorted
                  rows <-  mapM (mkRow False) grouped 
                  body <-  mapM mkBody rows
                  let body' = ListE body 
                  let typeString = LitE . stringL . nameBase $ nm
                  let varX = (VarE . mkName $ "x")
                  let varM = (VarE . mkName $ "m")
                  isTop <- if b then [| True |] else [| False |]
                  parent  <- [d|
                    instance PrettyPrint $(conT nm) where
                        pprint m x = makeLabelRow (T.pack $(return typeString)) $ foldr (<>) dashRow $(return body') 
                                |]
                  return $ (children <> parent)


getTypeName :: (Name,Type) -> Maybe Name
getTypeName (_,t) = case t of 
    ConT n -> Just n
    AppT ListT (ConT n) -> Just n
    _ -> Nothing

getTypeNamesVT :: [(Name,Type)] -> [Name]
getTypeNamesVT ns = case mapM getTypeName ns of
    Just x -> x 
    Nothing -> []


mkBody :: Either Exp [Exp] -> Q Exp
mkBody x = case x of
    Right xs -> do
        let xs' = return $  ListE xs
        [| makeDataRow $(xs') |]
    Left x -> [| $(return x) |]

type IsSum = Bool 

mkRow :: IsSum ->  Either (Name,Type) [(Name,Type)] -> Q (Either Exp [Exp])
mkRow b x = 
    case x of
        Left (n,t) -> do
            let lensNm = VarE $ if b then nameToPrismName n else nameToLensName n
            let varX = (VarE . mkName $ "x")
            let varM = (VarE . mkName $ "m")
            Left <$> [| pprint $(return varM) $ view $(return lensNm) $(return varX) |]
        Right nts -> do
            let lensStrings = map (\(a,b) -> LitE . stringL $ formatLens a) nts 
            let lensified = map (\(a,b) -> (VarE $ nameToLensName a,b)) nts
            let varX = (VarE . mkName $ "x")
            let varM = (VarE . mkName $ "m")
            Right <$> zipWithM (go varX varM) lensStrings lensified
   where
       go :: Exp -> Exp -> Exp -> (Exp,Type) -> Q Exp
       go varx varm lstr (lensE,_) = [| (T.pack $(return lstr)) <> ": " (pprint $(return varm) $ $(return varx) ^. $(return lensE))  |]



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

 
sortRecords :: (Name,Type) -> Q (Either (Name,Type) (Name,Type))
sortRecords (n,t) = do
    case t of
        (ConT x) -> do
            isPrim <- isInstanceOf x ''Primitive
            if isPrim 
                then return $ Right (n,t)
                else return $ Left  (n,t)
        _ -> return $ Left (n,t)
