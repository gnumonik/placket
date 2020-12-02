{-# LANGUAGE OverloadedStrings #-}

module Eval where 

import Syntax 
import BaseDefs
import Check
import Parse
import qualified Data.Map.Strict  as Map 
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Control.Lens 
import qualified Data.Text as T
import Data.Either 

unsafeFromRight (Right x) = x 

unsafeEval x = testEval $ unsafeFromRight (parseDSL' x)

type EvalError = T.Text 
type Evaluator = StateT DSLEnv Identity

lookupVar :: Sym -> Evaluator (Maybe Expr)
lookupVar sym = do
  defs <- view defEnv <$> get 
  return $ Map.lookup sym defs 
  
testEval :: Expr -> Expr
testEval x = runIdentity $ evalStateT (evaluate x) initEnv 


evaluate :: Expr -> Evaluator Expr
evaluate ex = do

  s <-  get

  case nf' ex of

      Var sym         -> do
        v <- lookupVar sym
        case v of
          Just e  -> nf' <$> evaluate e
          Nothing -> return $ Var sym 

      x1 :$: x2       -> do
        x1' <- nf' <$> evaluate x1
        x2' <- nf' <$> evaluate x2
        return $ nf' (nf' x1' :$: nf' x2')

      Lam s t ex      -> do
        ex' <- nf' <$> evaluate ex
        return $ Lam s t $ nf' ex' 

      Cons x1 x2      -> do
        x1' <- nf' <$> evaluate x1
        x2' <- nf' <$> evaluate x2
        return $ Cons x1' x2'

      Nil t           -> return $ Nil t

      Yep x           -> do
        x' <- nf' <$> evaluate x
        return $ Yep x' 

      Nope t          -> return $ Nope t  

      Pair x1 x2      -> do
        x1' <- nf' <$> evaluate x1
        x2' <- nf' <$> evaluate x2
        return $ Pair x1' x2' 

      Unary op x1     -> do
        x1' <- nf' <$> evaluate x1
        return $ delta (Unary op x1')

      Binary op x1 x2 -> do
        x1' <- nf' <$> evaluate x1
        x2' <- nf' <$> evaluate x2
        return $ delta (Binary op x1' x2')

      Unit  -> return Unit 

      MchBldr m x -> do
        x' <- nf' <$> evaluate x
        return $ MchBldr m x' 

      Lit x -> return $ Lit x 

      


