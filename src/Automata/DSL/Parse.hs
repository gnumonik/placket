{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}

module Parse where 

import PrimParsers
import Syntax
import qualified Data.Text as T 
import Text.Megaparsec 
import BaseDefs 
import Text.Megaparsec.Char.Lexer hiding (lexeme, binary)
import Text.Megaparsec.Char 
import Control.Monad.Combinators.Expr
import Control.Monad
import qualified FactoryTypes as FT 
import RecordParsers 
import ArgumentParsers 
import Data.Map (Map)
import qualified Data.Map as Map 

expr' :: T.Text -> Either T.Text Expr
expr' = parseLex (expr defMap)

term :: Map T.Text Expr -> Parser Expr
term m = pairExpr m
     <|> parens (expr m)
     <|> lamExpr m
     <|> listExpr m
     <|> binOp m
     <|> unaryOp m
     <|> lit
     <|> yepExpr m
     <|> nopeExpr 
     <|> namedFunction m
     <|> unit 
     <|> varExpr'

expr :: Map T.Text Expr ->  Parser Expr
expr m = lexeme $ try $ do
  ts <- some (term m)
  return $ foldr1 (\x y -> x :$: y) ts 


unaryOp :: Map T.Text Expr -> Parser Expr
unaryOp m = lexeme $ try $ do
  void . lexeme $ string "not"
  x <- notUnaryOp
  return $ Unary BOOL_NOT x
 where
   notUnaryOp 
      =  pairExpr m
      <|> parens  
            (pairExpr m
            <|> lamExpr m 
            <|> listExpr m 
            <|> binOp m 
            <|> lit
            <|> yepExpr m 
            <|> nopeExpr 
            <|> namedFunction m
            <|> unit
            <|> varExpr' )
        <|>  (pairExpr m
        <|> lamExpr m 
        <|> listExpr m 
        <|> binOp m 
        <|> lit
        <|> yepExpr m 
        <|> nopeExpr 
        <|> namedFunction m
        <|> varExpr' 
        <|> unit
        <|> varExpr' )
    

namedFunction :: Map T.Text Expr -> Parser Expr
namedFunction m =
  let m' = map (\x -> string x :: Parser T.Text) . map fst . Map.toList $ m
      p  = choice m' 
  in lexeme $ try $ do
    parsed <- p
    case Map.lookup parsed m of
      Just ex -> return ex 
      Nothing -> fail $ "No def named " <> (T.unpack parsed) 


lamExpr :: Map T.Text Expr -> Parser Expr
lamExpr m = lexeme $ try $ do
  void . lexeme $ char '\\'
  (Var v) <- varExpr' 
  void . lexeme $ string "::"
  t    <- types
  void . lexeme $ string "->"
  ex   <- expr m
  return $ Lam v t ex 


varExpr' :: Parser Expr 
varExpr' = lexeme $ try $ do
  void $ string "#"
  first <- letterChar 
  rest  <- many (letterChar <|> digitChar)
  return $ Var . T.pack $ (first : rest)


pairExpr :: Map T.Text Expr ->  Parser Expr
pairExpr m  = lexeme $ try $ do
  void . lexeme $ char '('
  x1 <- expr m
  void . lexeme $ char ','
  x2 <- expr m
  void . lexeme $ char ')'
  return $ Pair x1 x2 

listExpr :: Map T.Text Expr ->  Parser Expr
listExpr m  = lexeme $ try $ do
  void . lexeme $ char '['
  xs <-  expr m `sepBy` (lexeme $ char ',')
  void . lexeme $ char ']'
  void . lexeme $ string "::"
  t  <- types
  return $ case xs of
    [] -> Nil t 
    _  -> foldr (\x y -> x `Cons` y) (Nil t) xs 

binOp :: Map T.Text Expr -> Parser Expr 
binOp  m = lexeme $ try $ do
  first <-   pairExpr m 
            <|>  parens (pairExpr m
                <|> lamExpr m
                <|> listExpr m
                <|> unaryOp m
                <|> lit
                <|> yepExpr m
                <|> nopeExpr 
                <|> namedFunction m
                <|> unit
                <|> varExpr') 
            <|>  (pairExpr m
                <|> lamExpr m
                <|> listExpr m
                <|> unaryOp m
                <|> lit
                <|> yepExpr m
                <|> nopeExpr 
                <|> namedFunction m
                <|> unit
                <|> varExpr') 

  s <- option Nothing (just binaryOp)
  case s of
    Nothing -> return first
    Just anOp -> do
      rest <- expr m
      return $ Binary anOp first rest 

yepExpr :: Map T.Text Expr ->  Parser Expr
yepExpr m = lexeme $ try $ do
  void . lexeme $ string "Yep"
  x <- expr m
  return $ Yep x
  
nopeExpr ::  Parser Expr
nopeExpr  = lexeme $ try $ do
  void . lexeme $ string "Nope"
  void . lexeme $ string "::"
  t <- types
  return $ Nope t 

parens :: Parser a -> Parser a
parens p = (between (lexChar '(') (lexChar ')') p)  






var :: Parser T.Text 
var =  lexeme $ try $ do
  first <- lexeme letterChar 
  rest  <- T.pack <$> (lexeme $ some (letterChar <|> digitChar))
  return $ first `T.cons` rest 

tag :: T.Text -> Parser a -> Parser a 
tag txt p = lexeme $ try $ do
  void . lexeme $ string txt 
  parsed <- p 
  return parsed 


lit :: Parser Expr
lit = (Lit . LitInt       <$> int)
  <|> (Lit . LitBool      <$> bool)
  <|> (Lit . LitPType     <$> tag "proto:" protocolType)
  <|> (Lit . LitPMode     <$> tag "pMode:" ppMode)
  <|> (Lit . LitSWMode    <$> tag "swMode:" switchMode)
  <|> (Lit . LitWMode     <$> tag "wMode:" writeMode)
  <|> (Lit . LitQString   <$> quotedString )
  <|> (Lit . LitOptString <$> opticStrings)
  <|> (Lit . LitFPath     <$> tag "path:" filePath)
  <|> (Lit . LitFType     <$> tag "fieldType:" aWord)
  <|> (Lit . LitDouble    <$> double)
  <|> (Lit . LitField     <$> tag "field:" field)



unit :: Parser Expr
unit  = lexeme $ try $ do
  void . lexeme $ string "()"
  return Unit 

binaryOp 
  = prsEQ 
  <|> prsNOTEQ 
  <|> prsGTE 
  <|> prsGT 
  <|> prsLTE 
  <|> prsLT 
  <|> prsPLUS 
  <|> prsMINUS  
  <|> prsTIMES  
  <|> prsDIV 
  <|> prsOR 
  <|> prsAND 
  where
    prsEQ  = lexeme $ try $ do
      void . lexeme $ string "=="
      return $  COMP_EQ 

    prsNOTEQ  = lexeme $ try $ do
      void . lexeme $ (string "/=" <> string "!=")
      return $  COMP_NOTEQ 

    prsGTE  = lexeme $ try $ do
      void . lexeme $ string ">="
      return $ COMP_GTE 

    prsGT  = lexeme $ try $ do
      void . lexeme $ string ">"
      return $ COMP_GT 

    prsLTE  = lexeme $ try $ do
      void . lexeme $ string "<="
      return $ COMP_LTE
  
    prsLT  = lexeme $ try $ do
      void . lexeme $ string "<"
      return $ COMP_LT 

    prsPLUS = lexeme $ try $ do
      void . lexeme $ char '+'
      return $ ARITH_PLUS

    prsMINUS = lexeme $ try $ do
      void . lexeme $ char '-'
      return $ ARITH_MINUS 

    prsTIMES = lexeme $ try $ do
      void . lexeme $ char '*'
      return $ ARITH_TIMES

    prsDIV = lexeme $ try $ do
      void . lexeme $ char '/'
      return $ ARITH_DIV 

    prsOR  = lexeme $ try $ do
      void . lexeme $ string "||"
      return $ BOOL_OR 

    prsAND  = lexeme $ try $ do
      void . lexeme $ string "&&"
      return $ BOOL_AND 


types :: Map T.Text Type -> Parser Type
types m = arrT <|> pairT <|> listT <|> yepT <|> basetype <|> adt m

adt :: Map T.Text Type -> Parser Type 
adt m = 
  let ts = map fst . Map.toList $ m  
      p  = choice $ map (\x -> string x :: Parser T.Text) ts
  in lexeme $ try $ do
    parsed <- p
    case Map.lookup parsed m of
      Just x  -> return x
      Nothing -> fail  "Impossible type"

  
arrT :: Map T.Text Type -> Parser Type 
arrT m = lexeme $ try $ do
  fst <- pairT m <|> listT m <|> yepT m<|> basetype <|> adt m
  s <- option Nothing (just $ lexeme $ string "->")
  case s of
    Nothing -> return fst
    Just "->" -> do 
      rest <- arrT m
      return $ fst :->: rest 

pairT :: Map T.Text Type ->  Parser Type
pairT m = lexeme $ try $ do
  void . lexeme $ char '('
  fst <- types m
  void . lexeme $ char ','
  snd <- types m
  void . lexeme $ char ')'
  return $ PairT fst snd 

listT :: Map T.Text Type -> Parser Type
listT m = lexeme $ try $ do
  void . lexeme $ char '['
  ts   <- types m
  void . lexeme $ char ']'
  return $ ListT ts 

yepT ::Map T.Text Type -> Parser Type
yepT m = lexeme $ try $ do
      void . lexeme $ string "Yep"
      t <- types m
      return $ YepT t 



basetype = 
  let tyStr = choice $ map (\x -> string x :: Parser T.Text) . map fst $ baseTypes   
      tyMap = Map.fromList baseTypes
  in lexeme $ try $ do
    t <- tyStr
    case Map.lookup t tyMap of
      Just x -> return x
      Nothing -> fail "error: impossible type"

prepDef :: Parser (T.Text,[Expr],T.Text)
prepDef = lexeme $ try $ do
  void . lexeme $ string "def"
  (Var nm)<- varExpr'
  vars <- many varExpr' 
  void $ lexChar '='
  body <- some anySingle
  return $ (nm,vars,T.pack body)

