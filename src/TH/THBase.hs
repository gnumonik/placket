{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module THBase where

import           Control.Lens.Internal.FieldTH
import           Control.Lens.TH

import           Data.Char                     (isDigit, isLower, toLower,
                                                toUpper)
import           Language.Haskell.TH           (DecsQ, Name, mkName, nameBase)
mkProtocolLenses :: Name -> DecsQ
mkProtocolLenses = makeFieldOptics protocolFieldRules

protocolFieldRules :: LensRules
protocolFieldRules = LensRules
  { _simpleLenses    = True
  , _generateSigs    = True
  , _generateClasses = True
  , _allowIsos       = False
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = \n -> Just (mkName ("Has" ++ nameBase n), mkName (map toLower $ nameBase n))
  , _fieldToDef      = protocolNamer}

protocolNamer :: FieldNamer
protocolNamer _ _  field = return (TopName $ toLName field)
    where

        toLName :: Name -> Name
        toLName x = mkName $ go False "" . dropWhile (\c -> isLower c || isDigit c) . dropWhile (=='_') $ nameBase x

        go :: Bool -> String ->  String -> String
        go _     acc  []    = acc
        go False acc (x:xs) =
            if x == '_'
                then go True acc  xs
                else go False (acc ++ [toLower x]) xs
        go True acc (x:xs) = go False (acc ++ [x]) xs

