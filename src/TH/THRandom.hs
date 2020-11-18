{-# LANGUAGE RankNTypes, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module THRandom where 

import THUtils 
import Language.Haskell.TH 
import Classes 
import Generics.SOP 
import Data.Default 


gRandom :: forall a. (Generic a, AllN SOP Randomize (Code a), Default a) => Randomizer a
gRandom =  fmap to $  hctraverse' (Proxy @Randomize) ((\_ ->  I <$> random)) $ from (def  @a)


deriveRandomize :: Name -> DecsQ
deriveRandomize nm 
    = [d|
            instance Randomize $(conT nm) where
                random = gRandom
        |]
