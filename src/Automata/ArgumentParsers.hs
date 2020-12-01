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
import RecordTypes
import qualified Data.Vector as V

bool :: Parser Bool
bool = true <|> false 
  where
    true = lexeme $ try $ do
      void . lexeme $ string "True"
      return True 

    false = lexeme $ try $ do
      void . lexeme $ string "False"
      return False 

prefix :: T.Text -> Maybe a ->  Parser a -> Parser a
prefix str a p = case a of
  Nothing -> try p <|> go
  Just a' -> try p <|> option a' go 
 where 
   go = lexeme $ try $ do
    void . lexeme $ string str
    p

switchMode :: Parser SwitchMode
switchMode = reset <|> blow
  where
    reset = lexeme $ try $ do
      void . lexeme $ string "reset"
      return Reset

    blow = lexeme $ try $ do 
      void . lexeme $ string "blow"
      return Blow

ppMode :: Parser PrintMode
ppMode =  lexeme $ try $ do
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

reducePSelExp :: ProtocolSelectorExp -> Either T.Text Predicate
reducePSelExp pSelXP 
    = evalProtoSelectExp pSelXP Nothing >>= \x -> 
        return $ apFilter . mkCompare  $ x    


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

caseParser :: Parser (ProtocolSelectorExp, MachineArrow T.Text)
caseParser = lexeme $ try $ do
    myPredicate <- protocolSelectorExp
    void $ lexeme $ string "=>"
    mchArr <- machineArrParens 
    return $! (myPredicate, mchArr)

reduceCases :: [(ProtocolSelectorExp, MachineArrow T.Text)] -> Either T.Text [(Predicate, MachineArrow T.Text)]
reduceCases = mapM reduceCase 

reduceCase :: (ProtocolSelectorExp, MachineArrow T.Text) -> Either T.Text (Predicate, MachineArrow T.Text)
reduceCase (p,m) = case reducePSelExp p of
  Left err -> Left err 
  Right aPred -> Right (aPred,m)

data NamedMachine = NamedMachine MachineName (MachineArrow T.Text) deriving (Show, Eq)

namedMachine :: Parser NamedMachine
namedMachine = lexeme $ try $ do
    name   <- lexeme $ some (digitChar <|> letterChar)
    void $ lexeme $ string "="
    mach <- machineArrow
    return $! NamedMachine (MachineName . T.pack $ name) mach 



machineArrParens :: Parser (MachineArrow T.Text)
machineArrParens = lexeme $ try $ do
    x <- recParens
    let x' = T.drop 1 $ T.reverse  (T.drop 1 . T.reverse $ x) 
    case parseLex machineArrow x' of
        Left err -> fail $ show err
        Right y -> return y 

machineArrow :: Parser (MachineArrow T.Text)
machineArrow = lexeme $ try $ do
            a <- untilArr
            aSymb <- voidEof <|> arrSymb
            case aSymb of
                "~>" -> do
                    rest <- machineArrow 
                    return $ a :~> rest
                "~+>" -> do
                    rest <- some machineArrParens
                    return $   a :~+> rest
                ":|"  -> return $  a :| ()

arrSymb :: Parser T.Text
arrSymb =  (lexeme $ try $ string "~>") <|> (lexeme $ try $ string "~+>") 



voidEof :: Parser T.Text
voidEof = lexeme $ try $ do
    _ <- eof
    return ":|"


untilArr :: Parser T.Text
untilArr = lexeme $ try $ do
    first <- lexeme $ manyTill anySingle (lookAhead arrSymb <|> lookAhead (string "(") <|> voidEof)
    sep   <- voidEof <|>  (lookAhead $ lexeme . try $ string "(") <|>  (lookAhead arrSymb)
    case sep of
        "(" -> do

            btwn <- recParens -- between (char '(') (char ')') (many $ satisfy (\x -> x /= ')')) 

            rest <- untilArr
            return $ T.pack first <>  btwn <> rest
        "[" -> do
            btwn <- manyTill anySingle (lookAhead . lexeme $ char '[')
            void . lexeme $ char ']' 
            rest <- untilArr
            return $ T.pack (first <> "[" <> btwn <> "]") <> rest
        _   -> return $ T.pack  first 

notSep :: Parser [Char]
notSep = lexeme . try $ many $ satisfy (\x -> x `notElem` ("()" :: String))

builder :: Parser a -> Parser (V.Vector a)
builder p = lexeme $ try $ do
    first <- (between 
            (lexeme $ char '[') 
            (lexeme $ char ']') 
            (p  `sepBy1` (lexeme $ char ';') )) <?> "Error: Expected a builder. A builder has the form [ <SOMETHING> ; <MAYBE SOMETHING ELSE> ]"
    return $! V.force $ V.fromList $ first 
        
-- This is a hack to allow higher or machines to work properly. "ignores" nested parentheses between top-level machine arrows
-- (Doesn't actually ignore them, parses them then reconstructs the string)
------
recParens :: Parser T.Text
recParens = lexeme $ try $ do 
    (void . lexeme $ open) <?> "Error: Expected an open paren '('"
    first <- manyTill anySingle (lookAhead open <|> lookAhead close)
    c <- lookAhead . lexeme $ open <|> close
    case c of
        '(' -> do
            child <- (some recParens) <?> "Error: Mismatched parentheses (maybe?)"
            rest  <- manyTill anySingle close <?> "Expected a close paren ')' "
            return $ "(" <> T.pack first <> T.concat child <> T.pack rest <> ")" 
        ')' -> do
            (void . lexeme $ close ) <?> "Error: Mismatched parentheses (maybe?)"
            return $ "(" <> T.pack first <> ")"


   where
       open :: Parser Char
       open = (lexeme . try $ char '(') 

       close :: Parser Char
       close = (lexeme . try $ char ')') 
