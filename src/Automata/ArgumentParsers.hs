{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module ArgumentParsers where

import FactoryTypes
import Text.Megaparsec 
import Text.Megaparsec.Char 
import PrimParsers
import RecordParsers
import Control.Monad
import Data.SOP.BasicFunctors
import qualified Data.Text as T
import FieldClasses 
import RecordFuncs
import PacketFilters ( apFilter, mkCompare )


prefix :: T.Text -> Maybe a ->  Parser a -> Parser a
prefix str a p = case a of
  Nothing -> go
  Just a' -> try p <|> option a' go 
 where 
   go = lexeme $ try $ do
    void . lexeme $ string str
    p

switchMode :: Parser SwitchMode
switchMode = option Blow (reset <|> blow)
  where
    reset = lexeme $ try $ do
      void . lexeme $ string "reset"
      return Reset

    blow = lexeme $ try $ do 
      void . lexeme $ string "blow"
      return Blow

ppMode :: Parser PrintMode
ppMode = option Dflt go 
  where
    go = lexeme $ try $ do
      void . lexeme $ string "mode="
      m <- lexeme $ string "hex" <|> string "bin" <|> string "default"
      case  m of
          "hex"     -> return Hex
          "bin"     -> return Bin
          "default" -> return Dflt
          _         -> fail $ "Expected a printMode value (hex, bin, default)"

double :: Parser Double
double = lexeme $ try $ do
  first <- option "0" (some digitChar)
  void $ char '.'
  second <- option "0" (some digitChar)
  return $ (read (first <> "." <> second) :: Double)

qsNoPrefix :: Parser T.Text
qsNoPrefix = option "" quotedString 

qsLabelPrefix :: Parser T.Text
qsLabelPrefix = option "" go <?> "Expected 'label=\"<SOME TEXT>\""
  where
    go = lexeme $ try $ do
      void . lexeme $ string "label="
      quotedString 


filePathPrefix :: Parser FilePath
filePathPrefix = lexeme $ try $ do
  void . lexeme $ string "filePath"
  filePath 

protoSelectorPredicate :: Parser (Either T.Text Predicate)
protoSelectorPredicate = do
    myPredicate <- protocolSelectorExp 
    return $ case evalProtoSelectExp myPredicate Nothing of
        Right f -> Right $ apFilter (mkCompare f)
        Left errs -> Left $  errs 


intOptOne :: Parser Int
intOptOne = option 1 int 

msgSelectorExpPred :: Parser (Either T.Text Predicate)
msgSelectorExpPred = lexeme $ try $ do
  msgSel <- msgSelectorExp
  let myPred = evalMsgSelectorExp Nothing msgSel
  return myPred 

waitPrefix :: Parser Int
waitPrefix = option 0 go
  where
    go = lexeme $ try $ do
      void . lexeme $ string "wait="
      int 

writeMode :: Parser WriteMode
writeMode = (wr <|> apnd) <?> "Error: Invalid writeMode. Valid writeModes are write or append."
    where
        wr :: Parser WriteMode
        wr = lexeme $ try $ do
            void $ string "write"
            return $ Write
        apnd :: Parser WriteMode
        apnd = lexeme $ try $ do
            (void $ string "append")
            return $ Append
