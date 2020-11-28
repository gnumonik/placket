{-# LANGUAGE OverloadedStrings #-}

-- Simply Typed Lambda Calculus --
module STLC where 

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


lexChar :: Char -> Parser Char 
lexChar = lexeme . char 

-- adapted from: http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html

type Sym = T.Text

data Expr
    = Var Sym
    | App Expr Expr
    | Lam Sym Type Expr
    | Lit Lit 
    | PrimF1 PrimFunc Expr 
    | PrimF2 PrimFunc Expr Expr
    | PrimF3 PrimFunc Expr Expr Expr 
    | PrimF4 PrimFunc Expr Expr Expr Expr 
    | PrimF5 PrimFunc Expr Expr Expr Expr Expr  
    | List [Expr]
    | Pred (Predicate' Expr) deriving (Show, Eq)

data Lit 
  =   LitIntegral Int 
    | LitPType T.Text 
    | LitPMode PrintMode 
    | LitWMode F.WriteMode
    | LitQString T.Text 
    | LitOptString [T.Text]
    | LitPBuilder ProtocolBuilder 
    | LitFPath FilePath
    | LitFBuilder FieldBuilder
    | LitFType T.Text 
    | LitFSelExp FieldSelectorExp
    | LitPSelExp ProtocolSelectorExp
    | LitMachine 
    | LitTime Int 
    | LitCase (ProtocolSelectorExp, F.MachineArrow T.Text)
    | LitDouble Double
    | LitMsgSelExpPlus MsgSelectorExp
    | LitMsgSelExp MsgSelectorExp
    | LitPcapLock ()
    | LitPcapHandle ()
    | LitDumpHandle ()
    | LitDisplayChan ()
    | LitSource 
    | LitMArr (F.MachineArrow T.Text)
    | LitField Field deriving (Show, Eq)


type Body = Expr 

data PrimFunc 
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

selectPF :: Expr 
selectPF = Lam "x" (Arrow PSelExpT MachineT) (App (Var "x") (PrimF1 MK_SELECT (Var "y")) )


data Type 
  =  Arrow Type Type
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
  |  PcapLockT 
  |  PcapHandleT 
  |  DumpHandleT 
  |  DisplayChanT
  |  SourceT 
  |  MArrT 
  |  ListT 
  |  PredT 
  |  PrimF1T
  |  PrimF2T
  |  PrimF3T
  |  PrimF4T
  |  PrimF5T
  |  FieldT
    deriving (Eq, Read, Show)


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

pSelExpr :: Parser TypedExpr
pSelExpr = lexeme $ try $ do
    sel <- protocolSelectorExp
    return $ (PSelExpT,Lit $ LitPSelExp sel)

intExpr :: Parser TypedExpr 
intExpr = (\x -> (IntT, Lit $ LitIntegral x )) <$> int 

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

pSelXPExpr :: Parser TypedExpr 
pSelXPExpr = typedExpr protocolSelectorExp PSelExpT (Lit . LitPSelExp)

protoBuilderExpr :: Parser TypedExpr 
protoBuilderExpr = typedExpr protocolBuilder ProtoBuilderT (Lit . LitPBuilder) 

fBuilderExpr :: Parser TypedExpr 
fBuilderExpr = typedExpr fieldBuilder FBuilderT (Lit . LitFBuilder) 

fTypeExpr :: Parser TypedExpr 
fTypeExpr = typedExpr aWord FTypeT (Lit . LitFType) 

fSelXPExpr :: Parser TypedExpr 
fSelXPExpr = typedExpr fieldSelectorExp FSelExpT (Lit . LitFSelExp) 

msgSelXPExpr :: Parser TypedExpr
msgSelXPExpr = typedExpr msgSelectorExp MsgSelExpT (Lit . LitMsgSelExp)

timeExpr :: Parser TypedExpr
timeExpr = typedExpr int TimeT (Lit . LitTime) 

caseExpr :: Parser TypedExpr
caseExpr = typedExpr caseParser CaseT (Lit . LitCase) 

doubleExpr :: Parser TypedExpr
doubleExpr = typedExpr double DoubleT (Lit . LitDouble) 

msgSelXPPlsExpr :: Parser TypedExpr
msgSelXPPlsExpr = typedExpr msgSelectorExpPlus MsgSelExpPlusT (Lit . LitMsgSelExpPlus)

pcapLockExpr :: Parser TypedExpr
pcapLockExpr = typedExpr (return ()) PcapLockT (Lit . LitPcapLock)

pcapHdlExpr :: Parser TypedExpr
pcapHdlExpr = typedExpr (return ()) PcapHandleT (Lit . LitPcapHandle) 

dumpHdlExpr :: Parser TypedExpr
dumpHdlExpr = typedExpr (return ()) DumpHandleT (Lit . LitDumpHandle)

displayChanExpr :: Parser TypedExpr
displayChanExpr = typedExpr (return ()) DisplayChanT (Lit . LitDisplayChan)


marrExpr :: Parser TypedExpr 
marrExpr = typedExpr machineArrParens MArrT (Lit . LitMArr) 


-- casesExpr = typedExpr caseParser CasesT LitCases 










-- Evaluator 
type Env = Map.Map Sym Value



data Value 
  = ValIO ()
  | VClosure Expr Env 









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
     Arrow at rt -> do
        ta <- tCheck r a
        when (ta /= at) $ Left "Bad function argument type"
        return rt
     _ -> Left "Non-function in application"
tCheck r (Lam s t e) = do
    let r' = extend s t r
    te <- tCheck r' e
    return $ Arrow t te

tCheck r (Lit x) = case x of
  LitIntegral _ -> Right IntT 
  LitPType _    -> Right ProtoT
  LitPMode _    -> Right PModeT 
  LitWMode _    -> Right WModeT
  LitQString _  -> Right QStringT
  LitOptString _ -> Right OpStringsT 
  LitPBuilder _  -> Right ProtoBuilderT 
  LitFPath _     -> Right FPathT 
  LitFBuilder _  -> Right FBuilderT
  LitFType _     -> Right FTypeT
  LitFSelExp _   -> Right FSelExpT 
  LitPSelExp _   -> Right PSelExpT 
  LitMachine     -> Right MachineT 
  LitTime _      -> Right TimeT 
  LitCase _      -> Right CaseT 
  LitDouble _    -> Right DoubleT 
  LitMsgSelExpPlus _ -> Right MsgSelExpPlusT
  LitMsgSelExp _     -> Right MsgSelExpT
  LitPcapLock ()     -> Right PcapLockT 
  LitPcapHandle ()   -> Right PcapHandleT 
  LitDumpHandle ()   -> Right DumpHandleT
  LitDisplayChan ()  -> Right DisplayChanT
  LitSource          -> Right SourceT 
  LitMArr   _        -> Right MArrT 
  LitField _         -> Right FieldT

tCheck _ (PrimF1 _ _) = Right PrimF1T 
tCheck _ (PrimF2 _ _ _) = Right PrimF2T
tCheck _ (PrimF3 _ _ _ _) = Right PrimF3T
tCheck _ (PrimF4 _ _ _ _ _) = Right PrimF4T
tCheck _ (PrimF5 _ _ _ _ _ _) = Right PrimF5T    
typeCheck :: Expr -> Either ErrorMsg Type
typeCheck e =
    case tCheck initialEnv e of
    Left msg -> Left $ "Type error:\n" <> msg
    Right t -> Right t