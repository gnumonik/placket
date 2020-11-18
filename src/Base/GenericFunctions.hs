{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module GenericFunctions where
import           Classes
import           Data.Char
import           Data.Default
import           Data.Proxy   (Proxy) 
import           Data.Tree
import           Generics.SOP

 
getDataTypeInfo :: forall a z. (Generic a, HasDatatypeInfo a, Code a ~ z) 
                => Proxy a 
                -> DatatypeInfo z
getDataTypeInfo _ = datatypeInfo (Proxy @a)

getRecordNamesNP :: forall a r. (HasDatatypeInfo a, Code a ~ '[r]) 
                 => Proxy a 
                 ->  NP (K String) r
getRecordNamesNP _ = hmap (K . fieldName) $ getFInfo $ getCInfo $ datatypeInfo (Proxy @a)
  where
    getCInfo :: HasDatatypeInfo a => DatatypeInfo (Code a) -> NP ConstructorInfo (Code a)
    getCInfo (ADT _ _ c) = c
    getCInfo _           = error "Only normal ADTs are supported"

    getFInfo :: NP ConstructorInfo (b : xs) -> NP FieldInfo b
    getFInfo = go . hd
      where
        go :: ConstructorInfo b -> NP FieldInfo b
        go (Record _ fields) = fields
        go _                 = error "Only record fields are supported"

records :: (HasDatatypeInfo a, Code a ~ '[xs]) => Proxy a -> [[Char]]
records = hcollapse . getRecordNamesNP 

makeProxy ::  t -> Proxy t
makeProxy _ = Proxy :: Proxy t

-- make default instances of protocols
mkDefaultBounded :: forall t. Bounded t => Proxy t -> t
mkDefaultBounded _ = minBound :: t

mkDefaultNum :: forall t. Num t => Proxy t -> t
mkDefaultNum _ = fromIntegral (0 :: Int) :: t

mkDefaultMonoid :: forall t. (Monoid t) => Proxy t -> t
mkDefaultMonoid _ = mempty :: t

-- head $ apInjs_POP  $ POP $

mkDefaultG :: forall t . (Generic t, HasDatatypeInfo t,   AllN SOP Default  (Code t)) 
           => Proxy t ->  t
mkDefaultG _ = case (toType $ apInjs_NP $ doop $  goConstrs $ getConstrs $ getDInfo (Proxy :: Proxy t)) of
  [] -> error "Couldn't construct default for this type. (This should never trigger)"
  (x:_) -> x
  where

    toType ::  [NS (NP I) (Code t)] -> [t]
    toType xs = map (to . SOP) xs

    getConstrs :: DatatypeInfo (Code t) -> NP ConstructorInfo (Code t)
    getConstrs xss = case xss of
      ADT _ _ cs     -> cs
      Newtype _ _ cs -> cs :* Nil

    getDInfo :: Proxy t -> DatatypeInfo (Code t)
    getDInfo prox = getDataTypeInfo prox

    goConstrs ::   NP ConstructorInfo (Code t) -> NP (NP (K ())) (Code t)
    goConstrs _ =    hcpure (Proxy :: Proxy (AllN NS Default)) (hpure (K ()))

    doop :: NP (NP (K ())) (Code t) -> NP (NP I) (Code t)
    doop xss = 
      hcpure (Proxy :: Proxy (AllN NS Default)) 
      ((fn $ \x -> hcmap (Proxy :: Proxy Default) mkDefConstr' x)) `hap` xss

    mkDefConstr' :: forall w.  Default w => K () w -> I w
    mkDefConstr' _ =  (I (def :: w))


mkDefault ::  (Generic t, HasDatatypeInfo t,  All2 Default (Code t), Code t ~ (x ': '[])) 
          => Proxy t -> t
mkDefault t = to $ SOP $ Z $ go (getRecordNamesNP t)
   where
     go ::  forall b. All Default b => NP (K String) b -> NP I b
     go xss = hcmap (Proxy :: Proxy Default) toDef  xss
        where
          toDef :: forall c a. Default c =>  K a c -> I c
          toDef (K a) = I (def a)


processString :: String -> String
processString x = go False "" . dropWhile (\c -> isLower c || isDigit c) . dropWhile (=='_') $ x
  where
        go :: Bool -> String ->  String -> String
        go _     acc  []    = acc
        go False acc (x':xs) =
            if x' == '_'
                then go True acc  xs
                else go False (acc ++ [toLower x']) xs
        go True acc (x':xs) = go False (acc ++ [x']) xs

{--
data PPOptions = HEX | DEC | BIN | BSTRING | DEF deriving (Show, Eq) -- actually implement this, maybe

mkFieldTreeG :: forall t. (All2 PrettyPrintable (Code t), Generic t, HasDatatypeInfo t) 
             => t 
             -> FieldTree
mkFieldTreeG t =  
  cleanup $ Node (LABEL $ getADTName t) $ zipWith zipTrees (getRecs t) (goSOP $ from t)
    where
      cleanup :: FieldTree -> FieldTree
      cleanup tr = case tr of
        Node (l) [Node (_) ys] -> cleanup $ Node l ys
        tr'                    -> tr'

      goSOP :: All2 PrettyPrintable xss => SOP I xss -> [FieldTree ]
      goSOP (SOP (Z x)) = [Node (LABEL $ getADTName t) (mkTree x)]
      goSOP (SOP (S x)) = goSOP (SOP x)

      mkTree :: All PrettyPrintable x => NP I x -> [FieldTree]
      mkTree xss = hcollapse $ hcmap (Proxy :: Proxy PrettyPrintable) (mapIK mkFieldTree) xss

      getRecs :: HasDatatypeInfo t => t -> [FieldTree]
      getRecs _ = goInfo $ datatypeInfo (Proxy :: Proxy t)

      goInfo :: DatatypeInfo (Code t) -> [FieldTree]
      goInfo xss = case xss of
        (ADT _ _ ci)      -> goConstrs ci
        (Newtype _  _ ci) -> goConstrs (ci :* Nil)

      goConstrs :: forall z. NP ConstructorInfo z -> [FieldTree]
      goConstrs Nil        = []
      goConstrs (x :* xss) = case x of
        (Constructor nm)  -> Node (CONTENT nm) []  : goConstrs xss
        Record nm npFinfo -> Node (CONTENT nm) (goRecords npFinfo) : goConstrs xss
        Infix nm _ _      -> Node (CONTENT nm) [] : goConstrs xss

      goRecords :: forall x. NP FieldInfo x -> [FieldTree]
      goRecords Nil = []
      goRecords (x :* xs) = Node (CONTENT $ processString (fieldName x)) [] : goRecords xs

      getADTName ::   t -> String
      getADTName _ = case datatypeInfo (Proxy :: Proxy t) of
        (ADT _ nm _)     -> nm
        (Newtype _ nm _) -> nm

      zipTrees :: FieldTree -> FieldTree -> FieldTree -- Should really rewrite this with traverse
      zipTrees (Node lbl fs) (Node lbl' fs') = Node (f lbl lbl') $
        if null fs || null fs'
          then (fs ++ fs')
          else (zipWith zipTrees fs fs')
        where
          f :: FieldTreeToken -> FieldTreeToken -> FieldTreeToken
          f t1 t2 = case (t1,t2) of
            (LABEL x, LABEL y)     -> LABEL $ if x == y then x else "ERROR TWO LABELS IN SAME POSITION: " ++ x ++ "," ++ y
            (LABEL x, _)           -> LABEL x
            (_, LABEL y)           -> LABEL y
            (CONTENT x, CONTENT y) -> CONTENT $ x ++ "=" ++ y
            (CONTAINER, y)         -> y
            (x, CONTAINER)         -> x

--}



