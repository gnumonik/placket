{-# LANGUAGE TemplateHaskell #-}
module RecordParsers where

import Text.Parsec
import Text.Parsec.Text
import RecordTypes 
import qualified Data.Text as T
import PrimParsers
import Data.Char
import Control.Monad
import THWrappers 


protocolStrs :: [String]
protocolStrs = $mkProtocolStrings -- ["ETH","ARP","IP4","TCP","ICMP","UDP","DNS"]

protocolType :: Parser T.Text
protocolType = lexeme $ try $ do
    typeString <- lexeme $ many1 $ satisfy (\x -> isLetter x || isDigit x)
    if (map toUpper typeString) `elem` protocolStrs
        then return $ T.pack $ (map toUpper typeString)
        else fail $ "Error: " <> typeString <> " is not a supported protocol."


-- Predicate parser. 
-----
betweenPs :: Parser a -> Parser a 
betweenPs p = between (lexeme $ char '(') (lexeme $ char ')') p

predicate :: Parser a -> Parser (Predicate' a)
predicate p =  (parseOR p) <|> (parseAND p) <|> parseNOT p <|> (parseATOM p) 
    where
        parseATOM prsr = lexeme $ try $ do
            parsed <- prsr
            return $! ATOM parsed 
        
        parseNOT prsr = lexeme $ try $ do
            void $ lexeme $ string "not"
            parsed <- betweenPs (parseAND prsr <|> parseOR prsr) <|> parseNOT prsr <|> parseATOM prsr  
            return $! NOT parsed

        parseAND prsr = lexeme $ try $ do
            first <- betweenPs (parseAND prsr <|> parseOR prsr) <|> parseNOT prsr <|> parseATOM prsr 
            void $ lexeme $ string "&&"
            second <- betweenPs (parseAND prsr <|> parseOR prsr) <|> parseNOT prsr <|> parseATOM prsr 
            return $! first :&&: second

        parseOR prsr = lexeme $ try $ do
            first <- betweenPs (parseAND prsr <|> parseOR prsr) <|> parseNOT prsr <|> parseATOM prsr 
            void $ lexeme $ string "||"
            second <- betweenPs (parseAND prsr <|> parseOR prsr) <|> parseNOT prsr <|> parseATOM prsr 
            return $! first :||: second

-- Parses a list used as a record selector dictionary 
------
opticStrings :: Parser OpticStrs
opticStrings= lexeme $ try $ do
     oStrs <- lexeme $ many1 (satisfy $ \x -> isLetter x || x == '.') 
     return $! opticStrToList $ T.pack oStrs
   where
    opticStrToList :: T.Text -> [T.Text]
    opticStrToList txt = map T.pack $ go [] [] (T.unpack txt)
        where
            go :: [String] -> String -> String -> [String]
            go strsAcc ""       "" = strsAcc
            go strsAcc charsAcc "" = strsAcc <> [charsAcc]
            go strsAcc charsAcc (x:xs) = if x == '.' then go (strsAcc ++ [charsAcc]) "" xs else go strsAcc (charsAcc <> [x]) xs

--Parses a value on the right side of a record selector expression. 
------
field :: Parser Field
field = nonContigSet <|> fieldRange <|> fieldSingVal

nonContigSet :: Parser Field
nonContigSet = lexeme $ try $ do
    nonContig <- atLeastTwo
    return $ NonContigSet nonContig

fieldRange :: Parser Field
fieldRange = lexeme $ try $ do
    fRange <- range
    case fRange of
        (x,y) -> return $ RangeOfVals x y

fieldSingVal :: Parser Field
fieldSingVal = lexeme $ try $ do
    sVal <- singleValue
    return $ SingleValue sVal


-- Protocol builder stuff. For setting fields or generating (parts of) packets.
------

protocolBuilder :: Parser ProtocolBuilder
protocolBuilder = lexeme $ try $ do
    pType  <- protocolType
    fBldXP <- fieldBuilderExp 
    return $! ProtocolBuilder pType fBldXP 

fieldBuilderExp :: Parser FieldBuilderExp
fieldBuilderExp = lexeme $ try $ do
    void . lexeme $ char '('
    myFields <- fieldBuilders <|> allDefaults 
    void . lexeme $ char ')'
    return $! myFields
   where
       allDefaults :: Parser FieldBuilderExp
       allDefaults = lexeme $ do
           return AllDefaults
       
       fieldBuilders = lexeme $ try $ do 
           fs <- many1 fieldBuilder
           return $! FieldBuilderExp fs 

fieldBuilder :: Parser FieldBuilder
fieldBuilder = lexeme $ try $ do
    oStrs <- opticStrings
    void $ lexeme $ char '='
    fld   <- field
    return $! FieldBuilder oStrs fld

-- Selector stuff. For performing boolean tests on record fields.
------ 

msgSelectorExpPlus :: Parser MsgSelectorExp
msgSelectorExpPlus = lexeme $ try $ do
    msgSels <- between (lexeme $ char '[') (lexeme $ char ']') $
                (msgSelectorPlus `sepBy1` (lexeme $ char ';'))
    return $ MsgSelectorExp msgSels 

msgSelectorPlus :: Parser MsgSelector
msgSelectorPlus = mSelPlus <|> msgWc
    where
        mSelPlus = lexeme $ try $ do 
            p <- protocolSelectorPlus
            return $ MsgSelector p

msgSelectorExp :: Parser MsgSelectorExp
msgSelectorExp = lexeme $ try $ do
    msgSels <- between (lexeme $ char '[') (lexeme $ char ']') $
                (msgSelector `sepBy1` (lexeme $ char ';'))
    return $ MsgSelectorExp msgSels 

msgSelector :: Parser MsgSelector
msgSelector = msgSel <|> msgWc
   where
       msgSel = lexeme $ try $ do
           p <- protocolSelector
           return $ MsgSelector p

msgWc :: Parser MsgSelector
msgWc = lexeme $ try $ do
           void . lexeme $ char '*'
           return $ MsgSelectorWC

protocolSelectorExpPlus :: Parser ProtocolSelectorExp
protocolSelectorExpPlus = lexeme $ try $ do
    p <- predicate (protocolSelectorPlus)
    return $ ProtocolSelectorExp p

protocolSelectorExp :: Parser ProtocolSelectorExp
protocolSelectorExp = lexeme $ try $ do
    p <- predicate (protocolSelector) 
    return $ ProtocolSelectorExp p

protocolSelectorPlus :: Parser ProtocolSelector
protocolSelectorPlus = lexeme $ try $ do
    pType   <- protocolType
    fSelExp <- fieldSelectorExpPlus
    return $! ProtocolSelector pType fSelExp  

protocolSelector :: Parser ProtocolSelector
protocolSelector = lexeme $ try $ do
    pType   <- protocolType
    fSelExp <- fieldSelectorExp
    return $! ProtocolSelector pType fSelExp  

fieldSelectorExpPlus :: Parser FieldSelectorExp
fieldSelectorExpPlus = lexeme $ try $ do
    void . lexeme $ char '('
    myFieldPred <- lexeme $ (many (predicate fieldSelectorPlus))
    void . lexeme $ char ')' 
    case myFieldPred of
        []  -> return $ FieldSelectorExpWC
        [x] -> return $ FieldSelectorExp $ x
        xs  -> return $ FieldSelectorExp $ foldr1 (:&&:) xs 

fieldSelectorExp :: Parser FieldSelectorExp
fieldSelectorExp = lexeme $ try $ do
    void . lexeme $ char '('
    myFieldPred <- lexeme $ (many (predicate fieldSelector))
    void . lexeme $ char ')' 
    case myFieldPred of
        [] -> return $ FieldSelectorExpWC 
        [x] -> return $ FieldSelectorExp $ x
        xs -> return $ FieldSelectorExp $ foldr1 (:&&:) xs 

-- Not every function can make sensible use of the RefVarExpr
fieldSelectorPlus :: Parser FieldSelector 
fieldSelectorPlus = lexeme $ try $ do
    oStrs       <- opticStrings
    myComp      <- comp
    myCompareTo <- (varExpr <|> literal <|> compareToWC)
    return $! FieldSelector oStrs myComp myCompareTo 

fieldSelector :: Parser FieldSelector 
fieldSelector =  lexeme $ try $ do
    oStrs       <- opticStrings
    myComp      <- comp
    myCompareTo <- literal <|> compareToWC 
    return $! FieldSelector oStrs myComp myCompareTo 

varExpr :: Parser CompareTo
varExpr = lexeme $ try $ do
        void $ lexeme $ string "$("
        oStr <- opticStrings
       -- myOp <- (just operation) <|> nothing
        void $ lexeme $ string ")"
        return $! RefVarExpr oStr Nothing --myOp

literal :: Parser CompareTo
literal = lexeme $ try $ do
    l <- singleValue
    return $! Literal . SingleValue $ l  

compareToWC :: Parser CompareTo
compareToWC = lexeme $ try $ do
    return $! CompareToWC 

comp :: Parser Comp
comp = eq' <|> noteq' <|> lt' <|> lte' <|> gt' <|> gte'
    where
        eq' :: Parser Comp
        eq' = lexeme $ try $ do
            void $ lexeme $ string "="
            return $! EQ'
        
        noteq' :: Parser Comp
        noteq' = noteq1 <|> noteq2
            where
                noteq1 :: Parser Comp
                noteq1 = lexeme $ try $ do
                    void $ lexeme $ string "!="
                    return $! NOTEQ'

                noteq2 :: Parser Comp
                noteq2 = lexeme $ try $ do
                    void $ lexeme $ string "/="
                    return $! NOTEQ'

        lt' :: Parser Comp
        lt' = lexeme $ try $ do
            void $ lexeme $ string "<"
            return $! LT' 

        lte' :: Parser Comp
        lte' = lexeme $ try $ do
            void $ lexeme $ string "<="
            return $! LTE'

        gt' :: Parser Comp
        gt' = lexeme $ try $ do
            void $ lexeme $ string ">"
            return $! GT'
        
        gte' :: Parser Comp
        gte' = lexeme $ try $ do
            void $ lexeme $ string ">="
            return $! GTE' 

operation :: Parser Operation
operation = plus <|> minus 
    where
        plus :: Parser Operation
        plus = lexeme $ try $ do
            void $ lexeme $ char '+'
            myWord <- lexeme $ many1 (satisfy isDigit)
            return $! Plus (read myWord :: Word )

        minus :: Parser Operation
        minus = lexeme $ try $ do
            void $ lexeme $ char '-'
            myWord <- lexeme $ many1 (satisfy isDigit)
            return $! Minus (read myWord :: Word ) 