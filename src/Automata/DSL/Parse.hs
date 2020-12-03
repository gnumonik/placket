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
import Check
import Text.Megaparsec.Char.Lexer hiding (lexeme, binary, space)
import Text.Megaparsec.Char 
import Control.Monad.Combinators.Expr
import Control.Monad
import qualified FactoryTypes as FT 
import RecordParsers 
import ArgumentParsers 
import Data.Map (Map)
import Control.Monad.Except
import Control.Monad.Trans.State.Strict 
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO 
import Control.Monad.Identity
import Control.Lens

type Types = Map.Map Sym Type 

type Defs  = Map.Map Sym Expr 



parseAndCheck :: T.Text -> DSLMonad (Type,Expr)
parseAndCheck txt = do
  t <- getTyEnv
  ex <- liftEither $ parseDSL' txt
  tced <- liftEither $ tCheck t ex 
  return (tced,ex)
  


parseDSL' :: T.Text -> Either DSLError Expr
parseDSL' txt = runIdentity $ evalStateT (runExceptT $ parseExpr txt) initEnv




withTestHandler :: T.Text -> ExceptT (IO ()) (StateT DSLEnv Identity) Expr
withTestHandler txt = withExceptT testHandler (parseExpr txt)


testHandler :: T.Text -> IO ()
testHandler e =  TIO.putStr e 

parseExpr :: T.Text -> DSLMonad Expr 
parseExpr txt = do 
    t <- getTypes
    d <- getDefs 
    case parseLex (def d t) txt of
      Right (Def nm _ ex) -> do 
        lift . modify $ over defEnv $ Map.insert nm ex 
        return ex 
      Left err -> do 
        case parseLex (expr d t) txt of 
          Right ex -> return ex 
          Left err -> liftEither (Left err)



def :: Defs -> Types -> Parser Def
def d t = lexeme $ try $ do
  void . lexeme $ string "def"
  nm   <- var
  args <- many varExpr'
  void . lexeme $ char '='
  ex   <-  expr d t
  let argVars = map (\(Var x) -> x) args 
  let argTVars = zip args $ map T.singleton ['b'..'z']
  return $ Def nm argVars $ foldr (\((Var a),v) acc -> Lam a (TVar . TV $ v)  acc)  ex argTVars 
  
  


term :: Defs -> Types -> Parser Expr
term d t 
      =  namedFunction d
     <|> pairExpr d t
     <|> parens (expr d t)
     <|> binOp d t
     <|> lamExpr d t
     <|> listExpr d t
     <|> unaryOp d t
     <|> lit
     <|> yepExpr d t
     <|> nopeExpr t
     <|> unit
     <|> varExpr'

expr :: Defs -> Types ->  Parser Expr
expr d t = lexeme $ try $ do
  ts <- some (term d t)
  return $ foldr1 (\x y -> x :$: y) ts 




unaryOp :: Defs -> Types  -> Parser Expr
unaryOp d t = lexeme $ try $ do
  void . lexeme $ string "not"
  x <- notUnaryOp
  return $ Unary BOOL_NOT x
 where
   notUnaryOp 
      =  pairExpr d t 
      <|> parens  
            (namedFunction d 
            <|> pairExpr d t
            <|> binOp d t
            <|> lamExpr d t
            <|> listExpr d t
            <|> lit
            <|> yepExpr d t
            <|> nopeExpr t
            <|> unit
            <|> varExpr' )
        <|>  (namedFunction d 
        <|> pairExpr d t
        <|> binOp d t
        <|> lamExpr d t
        <|> listExpr d t
        <|> lit
        <|> yepExpr d t
        <|> nopeExpr t
        <|> varExpr' 
        <|> unit
        <|> varExpr' )
{--
lExpr :: Defs -> Types -> Parser Expr
lExpr d t = lexeme $ try $ do
  void $ char 'L'
  space
  ex <- expr d t
  return $ L ex 

rExpr :: Defs -> Types -> Parser Expr
rExpr d t = lexeme $ try $ do
  void $ char 'R'
  space
  ex <- expr d t
  return $ R ex 

-- write the reducer for this 
caseExpr :: Defs -> Types -> Parser Expr
caseExpr d t = lexeme $ try $ do
  void . lexeme $ string "Choice"
  void (lexChar 'L')
  void . lexeme $ string "=>" 
  x1 <- expr d t 
  void (lexChar ';')
  void (lexChar 'R')
  void . lexeme $ string "=>" 
  x2 <- expr d t
  return $ Choice  (Pair (L Unit) x1) (Pair (R Unit) x2) 
--}
    

namedFunction :: Defs -> Parser Expr
namedFunction d  =
  let m' = map (\x -> string x :: Parser T.Text) . map fst . Map.toList $ d
      p  = choice m' 
  in lexeme $ try $ do
    parsed <- p
    case Map.lookup parsed d of
      Just ex -> return ex 
      Nothing -> fail $ "No def named " <> (T.unpack parsed) 


lamExpr :: Defs -> Types -> Parser Expr
lamExpr d ty = lexeme $ try $ do
  void . lexeme $ char '\\'
  (Var v) <- varExpr' 
  t <- option (TVar (TV "x")) (annType ty)
  void . lexeme $ string "->"
  ex   <- expr d ty
  return $ Lam v t ex 

annType :: Types -> Parser Type 
annType ty = lexeme $ try $ do
  void . lexeme $ string "::"
  t <- types ty
  return t 


varExpr' :: Parser Expr 
varExpr' = lexeme $ try $ do
  void $ string "#"
  first <- letterChar 
  rest  <- many (letterChar <|> digitChar)
  return $ Var . T.pack $ (first : rest)


pairExpr :: Defs -> Types ->  Parser Expr
pairExpr d t  = lexeme $ try $ do
  void . lexeme $ char '('
  x1 <- expr d t
  void . lexeme $ char ','
  x2 <- expr d t
  void . lexeme $ char ')'
  return $ Pair x1 x2 

listExpr :: Defs -> Types ->  Parser Expr
listExpr d ty  = lexeme $ try $ do
  void . lexeme $ char '['
  xs <-  expr d ty `sepBy` (lexeme $ char ',')
  void . lexeme $ char ']'
  t <- option (TVar (TV "x")) (annType ty)
  return $ case xs of
    [] -> Nil t 
    _  -> foldr (\x y -> x `Cons` y) (Nil t) xs 

binOp :: Defs -> Types ->  Parser Expr 
binOp  d t = lexeme $ try $ do
  first <- notBinOp   
  s <- option Nothing (just binaryOp)
  case s of
    Nothing -> return first
    Just anOp -> do
      rest <- expr d t
      return $ Binary anOp first rest 
 where
   notBinOp = pairExpr d t
            <|>  parens (
                    namedFunction d 
                <|> pairExpr d t
                <|> lamExpr d t
                <|> listExpr d t
                <|> unaryOp d t
                <|> lit
                <|> yepExpr d t
                <|> nopeExpr t
                <|> unit
                <|> varExpr') 
            <|>  (namedFunction d 
                <|> pairExpr d t
                <|> lamExpr d t
                <|> listExpr d t
                <|> unaryOp d t
                <|> lit
                <|> yepExpr d t
                <|> nopeExpr t
                <|> unit
                <|> varExpr') 

yepExpr :: Defs -> Types ->   Parser Expr
yepExpr d t = lexeme $ try $ do
  void . lexeme $ string "Yep"
  x <- expr d t
  return $ Yep x
  
nopeExpr :: Types ->   Parser Expr
nopeExpr ty = lexeme $ try $ do
  void . lexeme $ string "Nope"
  void . lexeme $ string "::"
  t <- types ty
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
  <|> (Lit . LitOptString <$> between (lexeme . string $ "<") (lexeme . string $ ">") opticStrings)
  <|> (Lit . LitFPath     <$> tag "path:" filePath)
  <|> (Lit . LitFType     <$> tag "fieldType:" aWord)
  <|> (Lit . LitDouble    <$> double)
  -- <|> (Lit . LitField     <$> tag "field:" field)



unit :: Parser Expr
unit  = lexeme $ try $ do
  void . lexeme $ string "()"
  return Unit 

binaryOp :: Parser BinOp
binaryOp 
  =   prsPRECR
  <|> prsPRECL
  <|> prsEQ 
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

    prsPRECR = lexeme $ try $ do
      void . lexeme $ char '$'
      return $ PREC_RIGHT 

    prsPRECL = lexeme $ try $ do
      void . lexeme $ char '&'
      return $ PREC_LEFT 


types :: Map T.Text Type -> Parser Type
types m = arrT m <|> pairT m <|> listT m <|> yepT m <|> basetype <|> adt m <|> parens (types m)

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



basetype :: Parser Type
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

