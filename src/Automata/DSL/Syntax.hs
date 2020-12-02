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
import FieldClasses
import Data.List
import qualified Data.Text as T
import FieldClasses (Value, PrimToken, TypedVal, ValRangeSet, PrintMode)
import RecordTypes
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict 
import Control.Monad 
import Data.Word
import Classes
import qualified Data.ByteString as BS


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
    | L Expr
    | R Expr 
    | Choice Expr Expr  
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
  | BOOL_AND
  | PREC_RIGHT
  | PREC_LEFT  deriving (Show, Eq)


data Lit 
  =   LitInt Int
    | LitWord8 Word8 
    | LitWord16 Word16
    | LitWord24 Word24
    | LitWord32 Word32
    | LitIP4 IP4Address 
    | LitMac MacAddr
    | LitBString BS.ByteString
    | LitFlag Flag 
    | LitDName DNSName
    | LitMSGC MessageContent
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
    | LitField (ValRangeSet Lit, PrimToken) deriving (Show, Eq)





data Val 
  = ValMachine F.PacketMachine
  | ValSource F.PacketSrc 
  | ValFactory F.Factory 
  | ValClosure  Expr DSLEnv


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
  |  LR Type Type  
  |  DoubleT 
  |  SourceT 
  |  YepT  Type -- "Maybe Type"
  |  PairT Type Type 
  |  ListT Type
  |  PredT Type
  |  FieldT
  |  UnitT
  |  Type T.Text Type 
    deriving (Eq, Read, Show)
infixr 0 :->:


type TypedExpr = (Type,Expr)

newtype TypeEnv = TypeEnv (Map.Map Sym Type) deriving (Show, Eq)

data Def = Def Sym [Sym] Expr 

data DSLEnv = DSLEnv {_typeEnv :: TypeEnv
                     --,_valEnv  :: [Val]
                     ,_defEnv  :: Map.Map Sym Expr} deriving (Show, Eq)

type DSLError = T.Text 

type DSLMonad = ExceptT DSLError (StateT DSLEnv Identity) 
makeLenses ''DSLEnv

getTyEnv :: DSLMonad TypeEnv
getTyEnv = view typeEnv <$> lift get 

getTypes :: DSLMonad (Map.Map Sym Type)
getTypes = do
  (TypeEnv x) <- view typeEnv <$> lift get
  return x 
--getVals :: DSLMonad [Val]
--getVals = view valEnv <$> lift get

getDefs :: DSLMonad (Map.Map Sym Expr)
getDefs = view defEnv <$> lift get 


tShow :: Show a => a -> T.Text 
tShow x = T.pack . show $ x  

prettyType :: Type -> T.Text 
prettyType t = case t of
  Type txt _    -> txt 
  YepT t       -> "Yep " <> prettyType t 
  PairT t1 t2  -> "(" <> prettyType t1 <> "," <> prettyType t2 <> ")"
  ListT t      -> "[" <> prettyType t <> "]"
  PredT t      -> "Predicate " <> prettyType t
  t1 :->: t2   -> prettyType t1 <>  " -> " <> prettyType t2
  x            -> tShow x 


rType :: Type -> Type 
rType t = case t of
  ty1 :->: ty2 -> rType ty2
  ty           -> ty 

wrap :: T.Text -> T.Text
wrap txt = "(" <> txt <> ")"

prettyExpr :: Expr -> T.Text 
prettyExpr e = "\n" <> (prettyLambda 0 e) <> "\n"

prettyLambda :: Int -> Expr -> T.Text 
prettyLambda n e =  case e of
  (Var s)      -> s 
  (f :$: a)    -> prettyLambda n f <> " $ " <> prettyLambda n a 
  (Lam i t e ) ->  "\\" 
               <> i <> "::" <> (prettyType t)
               <> " -> \n" 
               <>   T.concat  (replicate (n + 1)  "      ") <> prettyLambda (n+1) e
  Unit            -> "()"
  Lit x           -> prettyLit x 
  Yep x           -> "Yep " <> prettyLambda n x
  Nope t          -> "Nope::" <> (prettyType t )
  Nil t           -> "[]::"  <> (prettyType t )
  Pair ex1 ex2    -> "(" <> prettyLambda n ex1 <> "," <> prettyLambda n ex2 <> ")"
  MchBldr m x     -> 
    "Machine Builder (" <> (T.pack . show $ m) <> ") " <> prettyLambda n x    
  Cons x1 x2      -> prettyList 0 (Cons x1 x2)
  Unary op x      -> prettyUnary n op x 
  Binary op x1 x2 -> prettyBinary n op x1 x2 
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

    prettyUnary :: Int -> UnOp -> Expr -> T.Text
    prettyUnary n o ex = case o of
      BOOL_NOT -> "not " <> prettyLambda n ex
    
    prettyBinary :: Int -> BinOp -> Expr -> Expr -> T.Text 
    prettyBinary n bOp x1 x2 = case bOp of
      ARITH_DIV   -> wrap $ prettyLambda n x1 <> " / " <> prettyLambda n x2 

      ARITH_MINUS -> wrap $ prettyLambda n x1 <> " - " <> prettyLambda n x2

      ARITH_PLUS  -> wrap $ prettyLambda n x1 <> " + " <> prettyLambda n x2 

      ARITH_TIMES -> wrap $ prettyLambda n x1 <> " * " <> prettyLambda n x2

      COMP_EQ        -> wrap $ prettyLambda n x1 <> " == " <> prettyLambda n x2

      COMP_NOTEQ     -> wrap $ prettyLambda n x1 <> " != " <> prettyLambda n x2

      COMP_GT      -> wrap $ prettyLambda n x1 <> " > " <> prettyLambda n x2

      COMP_GTE     -> wrap $ prettyLambda n x1 <> " >= " <> prettyLambda n x2

      COMP_LT      -> wrap $ prettyLambda n x1 <> " < " <> prettyLambda n x2

      COMP_LTE     -> wrap $ prettyLambda n x1 <> " <= " <> prettyLambda n x2

      BOOL_AND     -> wrap $ prettyLambda n x1 <> " && " <> prettyLambda n x2

      BOOL_OR      -> wrap $ prettyLambda n x1 <> " || " <> prettyLambda n x2 

      PREC_RIGHT   -> wrap $ prettyLambda n x1 <> " $ " <> prettyLambda n x2 

      PREC_LEFT    -> wrap $ prettyLambda n x1 <> " & " <> prettyLambda n x2 

    

    
  


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


-- Delta Reduction for Binary Operations 
delta2 :: Expr -> Expr 
delta2 ex@(Binary ARITH_DIV x y) = case (delta x, delta y) of 
  (Lit (LitInt x'), Lit (LitInt y')) -> Lit $ LitInt (x' `div` y')  
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitDouble (x' / y')
  _ -> ex 

delta2 ex@(Binary ARITH_TIMES x y) = case (delta x, delta y) of 
  (Lit (LitInt x'), Lit (LitInt y')) -> Lit $ LitInt (x' *  y')  
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitDouble (x' * y')
  _ -> ex 

delta2 ex@(Binary ARITH_PLUS x y) = case (delta x, delta y) of 
  (Lit (LitInt x'), Lit (LitInt y')) -> Lit $ LitInt (x' +  y')  
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitDouble (x' + y')
  _ -> ex 

delta2 ex@(Binary ARITH_MINUS x y) = case (delta x, delta y) of 
  (Lit (LitInt x'), Lit (LitInt y')) -> Lit $ LitInt (x' -  y')  
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitDouble (x' - y')
  _ -> ex 

delta2 ex@(Binary COMP_EQ x y)    = case (delta x,delta y) of
  (Lit (LitInt x'), Lit (LitInt y'))       -> Lit $ LitBool (x' == y')
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitBool (x' == y')
  _                                        -> ex 

delta2 ex@(Binary COMP_NOTEQ x y)    = case (delta x, delta y) of
  (Lit (LitInt x'), Lit (LitInt y'))       -> Lit $ LitBool (x' /= y')
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitBool (x' /= y')
  _                                        -> ex 

delta2 ex@(Binary COMP_GT x y)    = case (delta x,delta y) of
  (Lit (LitInt x'), Lit (LitInt y'))       -> Lit $ LitBool (x' > y')
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitBool (x' > y')
  _                                        -> ex 

delta2 ex@(Binary COMP_GTE x y)    = case (delta x,delta y) of
  (Lit (LitInt x'), Lit (LitInt y'))       -> Lit $ LitBool (x' >= y')
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitBool (x' >= y')
  _                                        -> ex 


delta2 ex@(Binary COMP_LT x y)    = case (delta x,delta y) of
  (Lit (LitInt x'), Lit (LitInt y'))       -> Lit $ LitBool (x' < y')
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitBool (x' < y')
  _                                        -> ex 

delta2 ex@(Binary COMP_LTE x y)    = case (delta x,delta y) of
  (Lit (LitInt x'), Lit (LitInt y'))       -> Lit $ LitBool (x' <= y')
  (Lit (LitDouble x'), Lit (LitDouble y')) -> Lit $ LitBool (x' <= y')
  _                                        -> ex   

delta2 ex@(Binary BOOL_AND x y)    = case (delta x,delta y) of
  (Lit (LitBool x'), Lit (LitBool y'))       -> Lit $ LitBool (x' && y')
  _                                        -> ex   

delta2 ex@(Binary BOOL_OR x y)    = case (delta x,delta y) of
  (Lit (LitBool x'), Lit (LitBool y'))       -> Lit $ LitBool (x' || y')
  _                                        -> ex

delta2 (Binary PREC_RIGHT x y) =  nf' $ delta x :$: (delta . nf'  $ y)

delta2 (Binary PREC_LEFT x y)  =  nf' $ (delta . nf' $ x)  :$: delta y

delta2 x = x 

delta1 :: Expr -> Expr
delta1 (Unary BOOL_NOT (Lit (LitBool x))) = Lit $ LitBool (not x)
delta1 x = x    


delta :: Expr -> Expr
delta ex = case ex of
  (Binary o x y) -> delta2 $ Binary o (delta x) (delta y) 
  (Unary _ _)    -> delta1 ex
  Lam s t e      -> Lam s t (delta e)
  Cons x1 x2     -> Cons (delta x1) (delta x2)
  Yep x          -> Yep . delta $ x 
  Pair x1 x2     -> Pair (delta x1) (delta x2) 
  x1 :$: x2      -> delta x1 :$: delta x2 
  _              -> ex 


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
  where spine (f :$: a) as = spine (f) (a:as)
        spine (Lam s t e) [] = Lam s t (nf e)
        spine (Lam s t e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = delta $ foldl (:$:) f (map nf as)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

nf' :: Expr -> Expr
nf' x = nf $ delta  (nf x)

initialEnv :: TypeEnv
initialEnv = TypeEnv Map.empty 

extend :: Sym -> Type -> TypeEnv -> TypeEnv
extend s t (TypeEnv r) = TypeEnv $ Map.insert s t r -- ((s, t) : r)

type ErrorMsg = T.Text 

type TC a = Either ErrorMsg a

findVar :: TypeEnv -> Sym -> TC Type
findVar (TypeEnv r) s =
    case Map.lookup s r of
    Just t -> return t
    Nothing -> Left $ "Cannot find variable " <> s



