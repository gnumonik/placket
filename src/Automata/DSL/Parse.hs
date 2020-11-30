{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}

module Parse where 

import PrimParsers
import Syntax
import qualified Data.Text as T 
import Text.Megaparsec 
import Text.Megaparsec.Char 
import Control.Monad
import qualified FactoryTypes as FT 
import RecordParsers 
import ArgumentParsers 

typedExpr :: Parser a -> Type -> (a -> Expr) -> Parser TypedExpr 
typedExpr prsr t c = lexeme $ try $ do
  parsed <- prsr
  return $ (t, c parsed)

varExpr :: Parser Expr 
varExpr = lexeme $ try $ do
  void $ string "VAR:"
  sym <- between (lexChar '(') (lexChar ')') $ some (letterChar <|> digitChar)
  return $ Var . T.pack $ sym 

ptypeExpr :: Parser TypedExpr
ptypeExpr = typedExpr protocolType ProtoT (Lit . LitPType) 

pModeExpr :: Parser TypedExpr
pModeExpr = typedExpr ppMode PModeT (Lit . LitPMode) 

wModeExpr :: Parser TypedExpr 
wModeExpr = typedExpr writeMode WModeT (Lit . LitWMode)

qStringExpr :: Parser TypedExpr
qStringExpr = typedExpr quotedString QStringT (Lit . LitQString)

fPathExpr :: Parser TypedExpr
fPathExpr = typedExpr filePath FPathT (Lit . LitFPath) 

fTypeExpr :: Parser TypedExpr 
fTypeExpr = typedExpr aWord FTypeT (Lit . LitFType) 

doubleExpr :: Parser TypedExpr
doubleExpr = typedExpr double DoubleT (Lit . LitDouble) 
