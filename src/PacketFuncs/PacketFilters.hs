{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module PacketFilters where
import           Staging
import           PrimTypes   ()
import           Wrappers    ()

import           Data.Monoid

type CompareResult = All
type HasProtocolType = Any

mkCompare :: (ProtocolMessage -> Maybe Bool) -> ProtocolMessage -> (CompareResult, HasProtocolType)
mkCompare f p = case f p of
    Just x  -> (All x,Any True)
    Nothing -> (All True, Any False )

apFilter :: Foldable t => (a -> (CompareResult, HasProtocolType)) -> t a -> Bool
apFilter f bld = case myFolded of
    (False,False) -> False
    xs            -> uncurry (&&) xs
   where
       myFolded = foldr (\x acc -> case f x of
           (All False, _) -> (False,False)
           (cRes,hsTyp)   -> (getAll cRes && fst acc, getAny hsTyp || snd acc)) (True,False) bld

 





