{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module PrettyPrint where

import Control.Monad.Trans.State.Strict
import Control.Monad.Identity


import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 
import Generics.SOP
import GenericFunctions 



dashes :: TL.Text
dashes =  TL.repeat '-'

hashes :: TL.Text
hashes = TL.repeat '#'

spaces :: TL.Text
spaces = TL.repeat ' '

dots  :: TL.Text
dots   = TL.intersperse ' ' $ TL.repeat '-'

dashRow :: T.Text
dashRow = "|" <> TL.toStrict (TL.take 78 dashes) <> "|\n"

dotRow :: T.Text 
dotRow = "|" <> TL.toStrict ( TL.take 78 $ TL.intersperse ' ' dots) <> "|\n"

spaceRow :: T.Text 
spaceRow = "|" <> TL.toStrict ( TL.take 78 $ spaces) <> "|\n"

spaceRow' :: T.Text
spaceRow' = (TL.toStrict $ TL.take 80 spaces) <> "\n"

takeL :: Int -> TL.Text -> T.Text 
takeL n lTxt = TL.toStrict $ TL.take (fromIntegral n) lTxt 

padDifference :: T.Text -> T.Text
padDifference str = let diff = 20 - T.length str 
                    in str <> takeL diff dashes
makeLabelRow :: T.Text -> T.Text
makeLabelRow str =   
  let str' = "<<<  " <> str <> "  >>>"
      paddashes = takeL ((78 - T.length str') `div` 2) dashes
  in if T.length str' `mod` 2 == 0 
      then "|" <> paddashes <> str' <> paddashes <> "|\n" 
      else "|" <> paddashes <> str' <> paddashes <> "-|\n"

makeLabelRowLJ :: T.Text -> T.Text
makeLabelRowLJ txt = 
  let txt' = txt <> " "
      remaining = 76 - T.length txt' 
  in "|- " <> txt' <> (takeL remaining dashes) <> "|\n" 
    

makeLabelRowDotted :: T.Text -> T.Text
makeLabelRowDotted str = 
  let str' = "<<<  " <> str <> "  >>>"
      paddots = takeL ((78 - T.length str') `div` 2) dots
  in if T.length str' `mod` 2 == 0 
      then "|" <> paddots  <> str' <> paddots <> "|\n" 
      else "|" <> paddots  <> str' <> paddots <> "-|\n"
 
    

makeDataRow :: [T.Text] -> T.Text
makeDataRow xs
    | null xs = "|" <> takeL 32 spaces <> "Nothing here!!" <> takeL 32 spaces <> "|\n"
    | otherwise = T.concat $ map go (splitFields maxCharsInRowC xs)
      where
        go :: [T.Text] -> T.Text
        go as
            | rowLen as == 78 = "|" <> rowStr as <> "|\n"
            | rowLen as < 78 = "|" <> rowStr as <> takeL (78 - rowLen as) spaces <> "|\n" -- 78 = the largest number of chars in a row after spaces applied
            | otherwise = error "ERROR!"

        rowStr :: [T.Text] -> T.Text
        rowStr as = applySpacer (gimmeResRem as) as

        rowLen :: [T.Text] -> Int
        rowLen as = T.length $ rowStr as

        gimmeResRem :: [T.Text] -> (Int, Int)
        gimmeResRem as = (78 - numCharsInFields as) `divMod` (length as + 1)

makeDataRowLJ :: [T.Text] -> T.Text
makeDataRowLJ xs
    | null xs = "|" <> takeL 32 spaces <> "Nothing here!!" <> takeL 32 spaces <> "|\n"
    | otherwise = T.concat $ map go (splitFields maxCharsInRowLJ xs)
      where
        go :: [T.Text] -> T.Text
        go as
            | rowLen as == 78 = "|" <> rowStr as <> "|\n"
            | rowLen as < 78 = "|" <> rowStr as <> takeL (78 - rowLen as) spaces <> "|\n" -- 78 = the largest number of chars in a row after spaces applied
            | otherwise = error "ERROR!"

        rowStr :: [T.Text] -> T.Text
        rowStr as = leftJustify (gimmeResRem as) as

        rowLen :: [T.Text] -> Int
        rowLen as = T.length $ rowStr as

        gimmeResRem :: [T.Text] -> (Int, Int)
        gimmeResRem as = (78 - numCharsInFields as) `divMod` length as

numFields :: [T.Text] -> Int
numFields = length

------------------------------------------------------------------
numCharsInFields :: [T.Text] -> Int
numCharsInFields as = sum (map T.length as)

maxCharsInRowLJ :: [T.Text] -> Int
maxCharsInRowLJ xs 
    = (78 -) 
    . foldr (\x y -> if x < y then x else y) 78 
    . filter (\x ->  (78 - numCharsInFields xs) - x > numFields xs) 
    . map fst $ filter (\x -> fst x >= numFields xs ) 
    . filter (\x -> (snd . snd) x == 0) 
    . zip [1..78] 
    $ map (\x -> (78 - numCharsInFields xs - x) `divMod` numFields xs) [1..78]

maxCharsInRowC :: [T.Text] -> Int
maxCharsInRowC xs 
    = (78 -) 
    . foldr (\x y -> if x < y then x else y) 78 
    . filter (\x ->  (78 - numCharsInFields xs) - x > (numFields xs + 1)) 
    . map fst 
    . filter (\x -> fst x >= (numFields xs + 1) ) 
    . filter (\x -> (snd . snd) x == 0) $ zip [1..78] 
    $ map (\x -> (78 - numCharsInFields xs - x) `divMod` (numFields xs + 1)) [1..78]

splitFields :: ([T.Text] -> Int) ->  [T.Text] -> [[T.Text]]
splitFields f strs = runSplitFields (go' (runChop strs) 0 [])
    where
     go' []       _     acc   = (acc,[])
     go' (q : qs) count acc
      | T.length q + count <= f (acc <> [q]) = go' qs (count + T.length q) (acc <> [q])
      | f [q] == 0                         = ([q], qs)
      | otherwise                          = (acc, q : qs)

     runSplitFields ([],[]) = []
     runSplitFields ([],b)  = [b]
     runSplitFields (a,b)   = a : runSplitFields (go' (runChop b) 0 [])

chop :: T.Text -> StateT [T.Text] Identity ()
chop str = do
    when (T.length str < 79) $
        modify $ \x -> x <> [str]
    unless(T.length str < 79) $ do
        modify $ \x -> x <> [T.take 78 str] -- values here SHOULD denote the largest T.Text size for a given field
        chop (T.drop 78 str)

runChop :: [T.Text] -> [T.Text]
runChop  xs = concat . runIdentity $ traverse chopChop xs
    where chopChop x = execStateT (chop x) []
-----------------------------------------------------------------

largestField :: [T.Text] -> Int
largestField = foldr ((\ x y -> if x > y then x else y) . T.length) 0

leftJustify :: (Int, Int) -> [T.Text] -> T.Text
leftJustify _        [y] = y <> takeL (78 - T.length y) spaces
leftJustify (res,rem') (y:ys)  
    = y <> if rem' > numFields ys 
        then leftJustify (res + newRes, newRem) ys 
        else T.concat $ map (\x -> x <> takeL res spaces) ys
    where
        (newRes,newRem) = rem' `divMod` numFields ys

applySpacer :: (Int, Int) ->  [T.Text] -> T.Text
applySpacer _ [a]
  | T.length a == 78 = a
  | T.length a `mod` 2 == 0 = takeL ((78 - T.length a) `div` 2) spaces <> a <> takeL ((78 - T.length a) `div` 2) spaces
  | otherwise = takeL ((78 - T.length a) `div` 2) spaces <> a <> " " <> takeL ((78 - T.length a) `div` 2) spaces
applySpacer (res, rem') ys = if rem' > (numFields ys + 1) then applySpacer (res + newRes, newRem) ys else (T.concat $ map (\x -> takeL res spaces <> x) ys) <> takeL res spaces
    where
        (newRes,newRem) = rem' `divMod` (numFields ys + 1)
{--
prettyByteString :: BS.ByteString -> T.Text -> T.Text
prettyByteString bs label = mappend (makeLabelRow label) (makeDataRow . map (prettyWord . concatMap (show . boolToBin) . word8ToBool) $ bwords bs)
    where
        word8ToBool w8 = map (testBit (w8 :: Word8)) [7,6,5,4,3,2,1,0]
        boolToBin x = if x then 1 else 0
        prettyWord xs = take 4 xs <> " " <> drop 4 xs
        bwords = BS.unpack
--}

