{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}
-- Simply Typed Lambda Calculus --
module Syntax where 

import Control.Lens hiding (List)
import Control.Monad 
import qualified FactoryTypes as F
import Control.Monad.Trans.Class 
import Control.Monad.Trans.Except
import Text.Megaparsec 
import Text.Megaparsec.Char 
import MachineParser 
import MyReaderT 
import BuilderMachines 
import EffectfulMachines
import HigherOrderMachines 
import SelectorMachines 
import ArgumentParsers 
import PrimParsers 
import RecordParsers 
import UtilityMachines
import Data.List
import qualified Data.Text as T
import FieldClasses (PrintMode)
import RecordTypes
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad 


{--    | Atom Expr 
    | Not Expr
    | Expr `And` Expr
    | Expr `Or` Expr 
    | Expr :>= Expr 
    | Expr :> Expr 
    | Expr := Expr
    | Expr :<= Expr 
    | Expr :< Expr 
    | Expr :!= Expr --}

-- We want: Type inference, variable binding, closures, environment

-- We DO NOT WANT: Recursion, polymorphism. 

-- Solution: "Neutered" simply typed lambda calculus. 

-- type FBuilder = PairT OpStringT FieldT
-- type FSel    = CompOp Expr Expr
-- type FSelExp = PredT (PairT CompOp (PairT Expr Expr))
-- type PSel    = PairT ProtoT (PairT )
-- type PSelXp  = Pred (ProtoT (CompOp Expr Expr))

lexChar :: Char -> Parser Char 
lexChar = lexeme . char 

-- adapted from: http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html

type Sym = T.Text


-- We can make all the sexier types as products of products using nested pairs. 
-- THANKFULLY we don't need sum types. Making this all work was hard enough. Dunno how
-- I'd implement that. 
data Expr
    = Var Sym
    | Expr :$: Expr -- I like the infix version more than 'App'. Looks more haskelley.
    | Lam Sym Type Expr
    | Lit Lit
    | Cons Expr Expr -- x : xs 
    | Nil Type -- []
    | Yep Expr -- "Just x"
    | Nope Type  -- "Nothing"
    | Pair Expr Expr -- (x,y)
    | Unary UnOp Expr 
    | Binary BinOp Expr Expr 
    | MchBldr MchBldr Expr
    | Unit 
      deriving (Show, Eq)
infixr 0 :$: 


data UnOp
  = BOOL_ATOM 
  | BOOL_NOT deriving (Show, Eq)


data BinOp
  = COMP_EQ
  | COMP_NOTEQ
  | COMP_GT
  | COMP_GTE
  | COMP_LT
  | COMP_LTE 
  | ARITH_PLUS
  | ARITH_MINUS 
  | ARITH_TIMES 
  | ARITH_DIV
  | BOOL_OR
  | BOOL_AND  deriving (Show, Eq)


data Lit 
  =   LitInt Int 
    | LitBool Bool 
    | LitPType T.Text 
    | LitPMode PrintMode
    | LitSWMode F.SwitchMode 
    | LitWMode F.WriteMode
    | LitQString T.Text 
    | LitOptString [T.Text]
    | LitFPath FilePath
    | LitFType T.Text 
    | LitDouble Double
    | LitField Field deriving (Show, Eq)

type Env = Map.Map Sym Val



data Val 
  = ValMachine F.PacketMachine
  | ValSource F.PacketSrc 
  | ValFactory F.Factory 
  | ValClosure  Expr Env 


type Body = Expr 

data MchBldr
  = MK_SELECT
  | MK_DISCARD  
  | MK_MAKERANDOM 
  | MK_PRETTYPRINT 
  | MK_PRINTFIELD 
  | MK_WRITEFIELD 
  | MK_POP 
  | MK_PUSH 
  | MK_EXTRACT 
  | MK_CUT 
  | MK_PULL 
  | MK_LIFT
  | MK_SET 
  | MK_CHECKSUM 
  | MK_MODIFYOPT
  | MK_INSERTOPT 
  | MK_DELETEOPT 
  | MK_ALERT 
  | MK_VOID 
  | MK_REPORT 
  | MK_CREATE 
  | MK_COUNT 
  | MK_BUFFER 
  | MK_DUMP 
  | MK_UNTIL 
  | MK_UNLESS 
  | MK_WHEN 
  | MK_AFTER 
  | MK_SWITCH 
  | MK_COUNTSWITCH 
  | MK_TIMESWITCH 
  | MK_CASE 
  | MK_LISTENFOR 
  | MK_LIMIT 
  | MK_GENERATE_S -- Need to split off the sources and add an expr 
  | MK_GENRANDOM_S 
  | MK_WHY_S 
  | MK_MK_TEA_S 
  | MK_LISTEN_S 
  | MK_READPCAP_S
    deriving (Show, Eq)

 


data Type 
  =  Type :->: Type
  |  UnaryOpT 
  |  BinaryOpT 
  |  BoolT 
  |  IntT 
  |  ProtoT 
  |  PModeT 
  |  WModeT 
  |  QStringT
  |  OpStringsT 
  |  SWModeT 
  |  FPathT
  |  FTypeT 
  |  MachineT 
  |  DoubleT 
  |  SourceT 
  |  YepT  Type -- "Maybe Type"
  |  PairT Type Type 
  |  ListT Type
  |  PredT Type
  |  FieldT
  |  UnitT
  |  ADT T.Text Type 
    deriving (Eq, Read, Show)




tShow :: Show a => a -> T.Text 
tShow x = T.pack . show $ x  

prettyType :: Type -> T.Text 
prettyType t = case t of
  ADT txt _    -> txt 
  YepT t       -> "Yep " <> prettyType t 
  PairT t1 t2  -> "(" <> prettyType t1 <> "," <> prettyType t2 <> ")"
  ListT t      -> "[" <> prettyType t <> "]"
  PredT t      -> "Predicate " <> prettyType t
  t1 :->: t2   -> prettyType t1 <>  " -> " <> prettyType t2
  x            -> tShow x 
infixr 0 :->:

rType :: Type -> Type 
rType t = case t of
  ty1 :->: ty2 -> rType ty2
  ty           -> ty 

type TypedExpr = (Type,Expr)

newtype TypeEnv = TypeEnv [(Sym, Type)] deriving (Show)

data Def = Def Sym [Sym] Expr 

data DSLEnv = DSLEnv {_typeEnv :: TypeEnv 
                     ,_exprEnv :: Env
                     ,_valEnv  :: [Val]
                     ,_defEnv  :: Map.Map Sym Expr}
makeLenses ''DSLEnv 

prettyExpr :: Expr -> T.Text 
prettyExpr e = "\n" <> (prettyLambda 0 e) <> "\n"

prettyLambda :: Int -> Expr -> T.Text 
prettyLambda n e =  case e of
  (Var s)   -> s 
  (f :$: a) -> prettyLambda n f <> " $ " <> prettyLambda n a 
  (Lam i t e ) ->  "\\" 
               <> i <> "::" <> (prettyType t)
               <> " -> \n" 
               <>   T.concat  (replicate (n + 1)  "      ") <> prettyLambda (n+1) e
  Unit         -> "()"
  Lit x        -> prettyLit x 
  Yep x        -> "Yep " <> prettyLambda n x
  Nope t       -> "Nope::" <> (prettyType t )
  Nil t        -> "[]::"  <> (prettyType t )
  Pair ex1 ex2 -> "(" <> prettyLambda n ex1 <> "," <> prettyLambda n ex2 <> ")"
  MchBldr m x  -> 
    "Machine Builder (" <> (T.pack . show $ m) <> ") " <> prettyLambda n x    
  Cons x1 x2   -> prettyList 0 (Cons x1 x2)
 where

    prettyLit :: Lit -> T.Text 
    prettyLit l = case l of
     LitInt  n      -> T.pack . show $ n 
     LitBool b      -> T.pack . show $ b 
     LitPType p     -> p 
     LitWMode w     -> T.pack . show $ w 
     LitQString q   -> q 
     LitOptString o -> 
       foldr (\x acc -> if T.null acc then x <> "" else x <> "." <> acc) "" o 
     LitFPath f     -> T.pack . show $ f
     LitDouble d    -> T.pack . show $ d
     LitField f     -> T.pack . show $ f
     LitSWMode m    -> T.pack . show $ m

    prettyList :: Int -> Expr -> T.Text 
    prettyList n l = "[" <> (go n l)
      where
        go :: Int -> Expr -> T.Text
        go _ (Nil t) = "]::" <> (prettyType t)
        go n (Cons x1 (Nil t)) = prettyLambda n x1 <> go n(Nil t)
        go n (Cons x xs ) = prettyLambda n x <> " , " <> go n xs  

    
  


whnf :: Expr -> Expr
whnf ee = spine ee []
  where spine (f :$: a) as = spine f  (a:as)
        spine (Lam s t e) (a:as) = spine (subst s a e) as
        spine f as = foldl (:$:) f as

freeVars :: Expr -> [Sym]
freeVars (Binary _ x1 x2) = freeVars x1 <> freeVars x2 
freeVars (Unary _ x)      = freeVars x 
freeVars (Var s)          = [s]
freeVars (f :$: a)        = freeVars f `union` freeVars a
freeVars (Lam i t e)      = freeVars e \\ [i]
freeVars Unit             = []
freeVars (Lit _)          = []
freeVars (Yep x)          = freeVars x 
freeVars (Nope t)         = []
freeVars (Nil t)          = []
freeVars (MchBldr _ x)    = freeVars x
freeVars (Cons x1 x2)     = freeVars x1 <> freeVars x2 


subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b
  where
      sub (Binary o x1 x2) = Binary o (sub x1) (sub x2)
      sub (Unary o x1)     = Unary o (sub x1)
      sub Unit             = Unit 
      sub (Yep x1)         = Yep (sub x1)
      sub (Nope t)         = Nope t 
      sub (Pair ex1 ex2)   = Pair (sub ex1) (sub ex2)
      sub (MchBldr m x1)   = MchBldr m (sub x1)
      sub (Cons x1 x2)     = Cons (sub x1) (sub x2)
      sub (Nil t)          = Nil t 
      sub (Lit x1)         = Lit x1 
      sub e@(Var i)        = if i == v then x else e
      sub (f :$: a)        = (sub f) :$: (sub a)
      sub (Lam i t e) =
          if v == i then
              Lam i t e
          else if i `elem` fvx then
              let i' = cloneSym e i
                  e' = substVar i i' e
              in  Lam i' t (sub e')
          else
              Lam i t (sub e)
      fvx = freeVars x
      cloneSym e i = loop i
          where loop i' = if i' `elem` vars then loop (i <> "'") else i'
                vars = fvx <> freeVars e

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v)   (Var v')    = v == v'
alphaEq (f :$: a) (f' :$: a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s t e) (Lam s' t' e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = spine ee []
  where spine (f :$: a) as = spine f (a:as)
        spine (Lam s t e) [] = Lam s t (nf e)
        spine (Lam s t e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = foldl (:$:) f (map nf as)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)



initialEnv :: TypeEnv
initialEnv = TypeEnv []

extend :: Sym -> Type -> TypeEnv -> TypeEnv
extend s t (TypeEnv r) = TypeEnv ((s, t) : r)

type ErrorMsg = T.Text 

type TC a = Either ErrorMsg a

findVar :: TypeEnv -> Sym -> TC Type
findVar (TypeEnv r) s =
    case lookup s r of
    Just t -> return t
    Nothing -> Left $ "Cannot find variable " <> s



