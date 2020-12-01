{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}

module Check where

import Syntax
import qualified Data.Text as T 
import Control.Monad 


prettyDef :: Def -> T.Text 
prettyDef (Def nm vars exp) = case tCheck initialEnv exp of
  Right r -> 
   "\n" <>  nm <> " :: " <> prettyType r <> "\n"
  <> nm <> " " <> (T.concat . map (<> " ") $ vars )
  <> " = \n  "
  <> prettyLambda 1 exp
  <> "\n\n"

tCheck :: TypeEnv -> Expr -> TC Type
tCheck r (Var s) =
    findVar r s
tCheck r (f :$: a) = do
    tf <- tCheck r f
    case tf of
     (at :->: rt) -> do
        ta <- tCheck r a
        when (ta /= at) $ Left "Bad function argument type"
        return rt
     _ -> Left "Non-function in application"
tCheck r (Lam s t e) = do
    let r' = extend s t r
    te <- tCheck r' e
    return $ t :->: te 

tCheck r (Lit x) = case x of
  LitInt   _     -> Right IntT 
  LitPType _     -> Right ProtoT
  LitPMode _     -> Right PModeT 
  LitWMode _     -> Right WModeT
  LitQString _   -> Right QStringT
  LitOptString _ -> Right OpStringsT 
  LitFPath _     -> Right FPathT 
  LitFType _     -> Right FTypeT
  LitDouble _    -> Right DoubleT 
  LitField _     -> Right FieldT
  LitBool _      -> Right BoolT
  LitSWMode _    -> Right SWModeT

tCheck r (Yep ex1) = case tCheck r ex1 of
  Right t  -> Right $ YepT t
  Left err -> Left err  

tCheck r (Nope t)  = Right $ YepT t 

tCheck r (Pair ex1 ex2) = case (tCheck r ex1, tCheck r ex2) of 
  (Right t, Right t') -> Right $ PairT t t'
  _                   -> Left $ "Error! Malformed type in tuple."

tCheck r (Nil t) = Right (ListT t) 

tCheck r (Cons ex1 (Nil t)) = case tCheck r ex1 of
  Right t' -> if t' == t 
                then Right (ListT t)
                else Left $ "Type Error: Cannot cons an element of type " 
                          <> (T.pack . show $ t') 
                          <> " onto a list of type <ListT "
                          <> (T.pack . show $ t) 
                          <> ">\n"
  Left err -> Left err 

tCheck r (Cons ex1 ex2) = case (tCheck r ex1, tCheck r ex2) of
  (Right t', Right (ListT t)) -> if t' == t 
                then Right (ListT t)
                else Left $ "Type Error: Cannot cons an element of type " 
                          <> (T.pack . show $ t') 
                          <> " onto a list of type <ListT "
                          <> (T.pack . show $ t) 
                          <> ">\n"
  (Right _, Right _)  -> Left $ "Type Error: Cannot cons \n" 
                            <> (T.pack . show $ ex1)
                            <> "\n"
                            <> "onto:"
                            <> (T.pack . show $ ex2)
                            <> "\n"
                            <> "Because ("
                            <>  (T.pack . show $ ex2)
                            <>  ") is not a list!"
  (Left err,_)        -> Left err 
  (Right _, Left err) -> Left err 

tCheck r (MchBldr _ ex) = case tCheck r ex of
  Right aType  -> Right $ MachineT
  Left  err    -> Left err 

tCheck _ Unit = Right UnitT 

tCheck r (Unary o x) = case o of
  BOOL_ATOM -> case tCheck r x of
    Right t  -> Right $ PredT t
    Left err -> Left err  

  BOOL_NOT-> case tCheck r x of
    Right t  -> Right $ PredT t
    Left err -> Left err  

tCheck r (Binary o x1 x2) = case o of
  COMP_EQ    -> goComp r x1 x2 
  COMP_NOTEQ -> goComp r x1 x2 
  COMP_GT    -> goComp r x1 x2 
  COMP_GTE   -> goComp r x1 x2 
  COMP_LT    -> goComp r x1 x2 
  COMP_LTE   -> goComp r x1 x2 

  ARITH_PLUS  -> goArith r x1 x2 
  ARITH_MINUS -> goArith r x1 x2 
  ARITH_TIMES -> goArith r x1 x2 
  ARITH_DIV   -> goArith r x1 x2 

  BOOL_OR     -> goPred r x1 x2
  BOOL_AND    -> goPred r x1 x2 
 where

   goComp r' x1' x2' = case mapM (tCheck r') [x1',x2'] of
     Right _ -> Right BoolT
     Left err     -> Left err 

   goArith r' x1' x2' = case mapM (tCheck r') [x1',x2'] of
     Right _      -> Right IntT
     Left err     -> Left err

   goPred r' x1' x2' = case mapM (tCheck r') [x1',x2'] of
     Right [t,t'] -> if t == t' 
                        then Right $ PredT t
                        else Left $ "Error! Type mismatch in expression:\n"
                                  <> (prettyExpr x1' <> tShow o <> prettyExpr x2')
                                  <> "\nCouldn't match type " 
                                  <> prettyType t
                                  <> " with type "
                                  <> prettyType t'
     Left err     -> Left err 
     Right _ -> Left "Error! Impossible type." 
 

typeCheck :: Expr -> TypeEnv -> Either ErrorMsg Type
typeCheck expr env  =
    case tCheck env expr of
    Left msg -> Left $ "Type error:\n" <> msg
    Right t -> Right t
