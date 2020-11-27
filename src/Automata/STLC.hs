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


lexChar :: Char -> Parser Char 
lexChar = lexeme . char 

-- adapted from: http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html

type Sym = T.Text

data Expr
    = Var Sym
    | App Expr Expr
    | Lam Sym Type Expr
    | LitIntegral Int 
    | LitPType T.Text 
    | LitMabInt (Maybe Int)
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
    | LitMachine F.PacketMachine 
    | LitTime Int 
    | LitCase (ProtocolSelectorExp, F.MachineArrow T.Text)
    | LitDouble Double
    | LitMsgSelExpPlus MsgSelectorExp
    | LitMsgSelExp MsgSelectorExp
    | LitPcapLock ()
    | LitPcapHandle ()
    | LitDumpHandle ()
    | LitDisplayChan ()
    | LitSource F.PacketSrc 
    | LitMArr (F.MachineArrow T.Text)
    | LitField Field
    | PrimF PrimFunc Expr ()
    | List [Expr]
    | Pred (Predicate' Expr)


type Body = Expr 

data PrimFunc 
  = SELECT
  | DISCARD  
  | MAKERANDOM 
  | PRETTYPRINT 
  | PRINTFIELD 
  | WRITEFIELD 
  | POP 
  | PUSH 
  | EXTRACT 
  | CUT 
  | PULL 
  | LIFT
  | SET 
  | CHECKSUM 
  | MODIFYOPT
  | INSERTOPT 
  | DELETEOPT 
  | ALERT 
  | VOID 
  | REPORT 
  | CREATE 
  | COUNT 
  | BUFFER 
  | DUMP 
  | UNTIL 
  | UNLESS 
  | WHEN 
  | AFTER 
  | SWITCH 
  | COUNTSWITCH 
  | TIMESWITCH 
  | CASE 
  | LISTENFOR 
  | LIMIT 
  | GENERATE_S 
  | GENRANDOM_S 
  | WHY_S 
  | TEA_S 
  | LISTEN_S 
  | READPCAP_S 





data Type 
  =  Arrow Type Type
  |  IntT 
  |  ProtoT 
  |  PModeT 
  |  WModeT 
  |  QStringT 
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
    return $ (PSelExpT,LitPSelExp sel)

intExpr :: Parser TypedExpr 
intExpr = (\x -> (IntT, LitIntegral x )) <$> int 

ptypeExpr :: Parser TypedExpr
ptypeExpr = typedExpr protocolType ProtoT LitPType 

pModeExpr :: Parser TypedExpr
pModeExpr = typedExpr ppMode PModeT LitPMode 

wModeExpr :: Parser TypedExpr 
wModeExpr = typedExpr writeMode WModeT LitWMode

qStringExpr :: Parser TypedExpr
qStringExpr = typedExpr quotedString QStringT LitQString

fPathExpr :: Parser TypedExpr
fPathExpr = typedExpr filePath FPathT LitFPath 

pSelXPExpr :: Parser TypedExpr 
pSelXPExpr = typedExpr protocolSelectorExp PSelExpT LitPSelExp

protoBuilderExpr :: Parser TypedExpr 
protoBuilderExpr = typedExpr protocolBuilder ProtoBuilderT LitPBuilder 

fBuilderExpr :: Parser TypedExpr 
fBuilderExpr = typedExpr fieldBuilder FBuilderT LitFBuilder 

fTypeExpr :: Parser TypedExpr 
fTypeExpr = typedExpr aWord FTypeT LitFType 

fSelXPExpr :: Parser TypedExpr 
fSelXPExpr = typedExpr fieldSelectorExp FSelExpT LitFSelExp 

msgSelXPExpr :: Parser TypedExpr
msgSelXPExpr = typedExpr msgSelectorExp MsgSelExpT LitMsgSelExp

timeExpr :: Parser TypedExpr
timeExpr = typedExpr int TimeT LitTime 

caseExpr :: Parser TypedExpr
caseExpr = typedExpr caseParser CaseT LitCase 

doubleExpr :: Parser TypedExpr
doubleExpr = typedExpr double DoubleT LitDouble 

msgSelXPPlsExpr :: Parser TypedExpr
msgSelXPPlsExpr = typedExpr msgSelectorExpPlus MsgSelExpPlusT LitMsgSelExpPlus

pcapLockExpr :: Parser TypedExpr
pcapLockExpr = typedExpr (return ()) PcapLockT LitPcapLock

pcapHdlExpr :: Parser TypedExpr
pcapHdlExpr = typedExpr (return ()) PcapHandleT LitPcapHandle 

dumpHdlExpr :: Parser TypedExpr
dumpHdlExpr = typedExpr (return ()) DumpHandleT LitDumpHandle

displayChanExpr :: Parser TypedExpr
displayChanExpr = typedExpr (return ()) DisplayChanT LitDisplayChan

sourceExpr :: Parser TypedExpr
sourceExpr = typedExpr (return ()) SourceT LitSource 

marrExpr :: Parser TypedExpr 
marrExpr = typedExpr machineArrParens MArrT LitMArr 


-- casesExpr = typedExpr caseParser CasesT LitCases 










-- Evaluator 
type Env = Map Sym Value



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

typeCheck :: Expr -> Either ErrorMsg Type
typeCheck e =
    case tCheck initialEnv e of
    Left msg -> Left $ "Type error:\n" <> msg
    Right t -> Right t