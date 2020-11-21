{-# LANGUAGE TemplateHaskell #-} 

module THOptional where

import THUtils 
import FieldClasses
import Control.Lens hiding (children)
import THRecords 
import Language.Haskell.TH 



isListT :: Type -> Bool
isListT (AppT ListT (ConT _ )) = True
isListT _                      = False 

deriveOptionalField :: Name -> DecsQ
deriveOptionalField n = do
    isPrim <- isPrimitive n
    if isPrim
        then return []
        else do
            isSum <- isSumType n
            TyConI dec <- reify n
            case dec of
                DataD _c _n _bnd _k dCons _ -> if isSum then return [] else goProd n dCons
                NewtypeD _c _n _bnd _k dCon _ -> goProd n [dCon] 

                _ -> fail $ "Error: Unsupported type."

   where

       goProd :: Name -> [Con] -> DecsQ
       goProd nm cs = do 
           let vts =      filter (\x -> isListT . snd $ x) 
                        . (concatMap . map) (\(v,b,t) -> (v,t))
                        . map (\(RecC _ vbts) -> vbts) 
                        . filter isRecC $ cs
           concat <$> mapM (go nm) vts 
        where
            go :: Name -> (Name,Type) -> DecsQ
            go parentName (opticName,myType) = do
                let optic = VarE . nameToLensName $ opticName
                let childType = (\(AppT ListT (ConT n)) -> n) myType
                [d| 
                    instance OptionalFieldOf $(conT parentName) $(conT childType) where
                        insertField $(varP . mkName $ "par") $(varP . mkName $ "chld") =
                            over  $(return optic) (<> chld) par
                        deleteFieldIf $(varP . mkName $ "f") $(varP . mkName $ "par") = 
                            over $(return optic) (filter $ \x -> not . f $ x) par
                        modifyFieldIf $(varP . mkName $ "par") $(varP . mkName $ "f") $(varP . mkName $ "g") = 
                            over $(return optic) (concatMap $ \x -> if f x then g x else [x]) par
                   |]