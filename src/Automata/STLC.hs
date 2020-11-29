{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}
-- Simply Typed Lambda Calculus --
module STLC where 

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


-- We want: Type inference, variable binding, closures, environment

-- We DO NOT WANT: Recursion, polymorphism. 

-- Solution: "Neutered" simply typed lambda calculus. 


lexChar :: Char -> Parser Char 
lexChar = lexeme . char 

-- adapted from: http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html

type Sym = T.Text

data Expr
    = Var Sym
    | App Expr Expr
    | Lam Sym Type Expr
    | Lit Lit
    | List [Expr]
    | FieldBuilder' Expr Expr
    | MchBldr MchBldr Expr 
      deriving (Show, Eq)

data Lit 
  =   LitInt Int 
    | LitBool Bool 
    | LitPType T.Text 
    | LitPMode PrintMode 
    | LitWMode F.WriteMode
    | LitQString T.Text 
    | LitOptString [T.Text]
    | LitFPath FilePath
    | LitFType T.Text 
    | LitCase (ProtocolSelectorExp, F.MachineArrow T.Text)
    | LitDouble Double
    | LitField Field deriving (Show, Eq)

type Env = Map.Map Sym Val

(|$) :: Expr -> Expr -> Expr 
a |$ b = App a b 

nil :: Expr
nil = List []



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
  | MK_GENERATE_S 
  | MK_GENRANDOM_S 
  | MK_WHY_S 
  | MK_MK_TEA_S 
  | MK_LISTEN_S 
  | MK_READPCAP_S
  | PLUS
  | MINUS 
  | TIMES deriving (Show, Eq)

 


data Type 
  =  Type :->: Type
  |  BoolT 
  |  IntT 
  |  ProtoT 
  |  PModeT 
  |  WModeT 
  |  QStringT
  |  OpStringsT 
  |  FPathT
  |  PSelExpT 
  |  ProtoBuilderT 
  |  FBuilderT 
  |  FTypeT 
  |  FSelExpT 
  |  MsgSelExpT 
  |  MachineT 
  |  TimeT 
  |  CaseT 
  |  DoubleT 
  |  MsgSelExpPlusT
  |  SourceT 
  |  MArrT 
  |  TupleT Type Type 
  |  ListT 
  |  PredT Type 
  |  FieldT
  |  UnitT
    deriving (Eq, Read, Show)




data Def = Def Sym Expr 

-- \select -> \x -> MK_SELECT x



mkSelect :: Expr  
mkSelect = Lam "x" (PSelExpT) 
              (MchBldr MK_SELECT (Var "x"))


mkFieldBuilder :: Expr 
mkFieldBuilder = Lam "ostrs" OpStringsT 
                  $ Lam "field" FieldT (FieldBuilder' (Var "ostrs") (Var "field")) 

type TypedExpr = (Type,Expr)

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







caseExpr :: Parser TypedExpr
caseExpr = typedExpr caseParser CaseT (Lit . LitCase) 

doubleExpr :: Parser TypedExpr
doubleExpr = typedExpr double DoubleT (Lit . LitDouble) 



-- Type System / LC basic implementation stuff 

whnf :: Expr -> Expr
whnf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s t e) (a:as) = spine (subst s a e) as
        spine f as = foldl App f as

freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i t e) = freeVars e \\ [i]

subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b
  where sub e@(Var i) = if i == v then x else e
        sub (App f a) = App (sub f) (sub a)
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
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s t e) (Lam s' t' e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s t e) [] = Lam s t (nf e)
        spine (Lam s t e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = foldl App f (map nf as)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)



newtype TypeEnv = TypeEnv [(Sym, Type)] deriving (Show)

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

tCheck :: TypeEnv -> Expr -> TC Type
tCheck r (Var s) =
    findVar r s
tCheck r (App f a) = do
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
  LitCase _      -> Right CaseT 
  LitDouble _    -> Right DoubleT 
  LitField _     -> Right FieldT

tCheck r (MchBldr _ ex) = case tCheck r ex of
  Right aType  -> Right $ MachineT
  Left  err    -> Left err 

tCheck r (FieldBuilder' os fl) = case (tCheck r os, tCheck r fl) of -- is this necessary?
  (Right OpStringsT,Right FieldT) ->  Right FBuilderT 
  _                               -> Left "Error! FieldBuilder expected <OPTICS STRINGS> <FIELD> but received something else!"

typeCheck :: Expr -> TypeEnv -> Either ErrorMsg Type
typeCheck expr env  =
    case tCheck env expr of
    Left msg -> Left $ "Type error:\n" <> msg
    Right t -> Right t

--}

data DSLEnv = DSLEnv {_typeEnv :: TypeEnv 
                     ,_exprEnv :: Env
                     ,_valEnv  :: [Val]}
makeLenses ''DSLEnv 