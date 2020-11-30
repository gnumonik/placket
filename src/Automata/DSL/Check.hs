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


typeCheck :: Expr -> TypeEnv -> Either ErrorMsg Type
typeCheck expr env  =
    case tCheck env expr of
    Left msg -> Left $ "Type error:\n" <> msg
    Right t -> Right t
