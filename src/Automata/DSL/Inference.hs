{-# LANGUAGE OverloadedStrings #-}

module Inference where 

import Syntax 
import Check
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set  
import Control.Monad.State.Strict 
import Control.Monad.Except
import qualified Data.Text as T
import Data.List (nub)
import BaseDefs 
import Eval 
import Check 
-- adapted from https://github.com/sdiehl/write-you-a-haskell/blob/master/006_hindley_milner.md




runDSL z = myTEnv >>= \x -> runInfer $ infer x  $ unsafeEval z

myTEnv :: Either TypeError TypeEnvI
myTEnv = initTEnvI baseDefs emptyTyenv

initTEnvI :: [Def] -> TypeEnvI -> Either TypeError TypeEnvI
initTEnvI [] acc = Right acc 
initTEnvI ((Def n _ e):ds) acc@(TypeEnvI ts) = 
  case runInfer (infer acc e) of
    Right sch -> initTEnvI ds (TypeEnvI $ Map.insert n sch ts)
    Left  err -> Left err 

data Unique = Unique { count :: Int }

type TypeError = T.Text 

data Scheme = Forall [TVar] Type deriving Show 

newtype TypeEnvI = TypeEnvI (Map Sym Scheme) deriving Show 

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

emptyTyenv :: TypeEnvI
emptyTyenv = TypeEnvI Map.empty

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

extendI :: TypeEnvI -> (Sym,Scheme) -> TypeEnvI
extendI (TypeEnvI e) (sym,sch) = TypeEnvI $ Map.insert sym sch e

type Infer a = ExceptT TypeError (State Unique) a

runInfer :: Infer (Subst,Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err -> Left err 
  Right res -> Right $ closeOver res 

type Subst = Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty 

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar 

instance Substitutable Type where
  apply s (YepT t)      = YepT (apply s t)
  apply s (PairT t1 t2) = PairT (apply s t1) (apply s t2)
  apply s (ListT t) = ListT (apply s t)
  apply s (PredT t1)    = PredT (apply s t1)
  apply s (Type txt t)  = Type txt (apply s t)
  apply s t@(TVar a)    = Map.findWithDefault t a s 
  apply s (t1 :->: t2)  = apply s t1 :->: apply s t2
  apply _ other         = other 

  ftv (YepT t)      = ftv t
  ftv (PairT t1 t2) = Set.union (ftv t1) (ftv t2) -- not sure about this
  ftv (ListT t1)    = ftv t1 
  ftv (PredT t1)    = ftv t1 
  ftv (Type txt t)  = ftv t 
  ftv (TVar t)      = Set.singleton t 
  ftv (t1 :->: t2)  = Set.union (ftv t1) (ftv t2)
  ftv _             = Set.empty 

instance Substitutable Scheme where 
  apply s (Forall as t) = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t)     = ftv t `Set.difference` Set.fromList as 

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply 
  ftv = foldr (Set.union . ftv) Set.empty 

instance Substitutable TypeEnvI where
  apply s (TypeEnvI e) = TypeEnvI $ Map.map (apply s) e 
  ftv     (TypeEnvI e) = ftv $ Map.elems e 

letters :: [T.Text]
letters = map T.pack letters'

letters' :: [String]
letters' = [1..] >>=  flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)


occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t 

unify :: Type -> Type -> Infer Subst
unify (t1 :->: t2) (t1' :->: t2') = do
  s1  <- unify t1 t1'
  s2  <- unify t2 t2' 
  return $ s2 `compose` s1 

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t

unify (YepT t) (YepT t') = unify t t' 


unify (PairT t1 t2) (PairT t1' t2') 
  = if t1 == t1' && t2 == t2'  
      then return nullSubst
       else throwError "error b!" 

unify (ListT t) (ListT t') = unify t t'

unify (PredT t) (PredT t') = unify t t'

unify (Type txt t) (Type txt' t') = unify t t'

unify UnitT UnitT = return nullSubst

unify MachineT MachineT = return nullSubst

unify t1 t2 = if t1 == t2 
  then return nullSubst 
  else throwError $ "\n" <> tShow t1 <> "\n" <> tShow t2 <> "\n\n"

bind :: TVar -> Type -> Infer Subst
bind a t | t == TVar a     = return nullSubst
         | occursCheck a t = throwError "Occurs check!"
         | otherwise       = return $ Map.singleton a t 

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t


generalize :: TypeEnvI -> Type -> Scheme
generalize env t  = Forall as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

infer :: TypeEnvI -> Expr -> Infer (Subst,Type)
infer en ex = case ex of

  Var x -> lookupEnv en x 

  Lam s t e -> case t of
    TVar _ -> do
      tv <- fresh
      let en' = en `extendI` (s, Forall [] tv)
      (s1,t1)  <- infer en' e 
      return (s1, apply s1 tv :->: t1)
    aType  -> return (nullSubst, aType) 

  e1 :$: e2 -> do
    tv <- fresh
    (s1,t1)  <- infer en e1
    (s2, t2) <- infer (apply s1 en) e2
    s3       <- unify (apply s2 t1) (t2 :->: tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Binary op e1 e2 -> do
    (s1, t1) <- infer en e1
    (s2, t2) <- infer en e2
    tv <- fresh
    s3 <- unify (t1 :->: (t2 :->: tv)) (ops Map.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)


  Cons ex1 ex2 -> do
    (s1, t1) <- infer en ex1
    (s2, t2) <- infer en ex2 
    tv <- fresh
    s3'     <- unify (apply s1 t2) (apply s2 t1)
    return $ (s3', apply s3' tv)




  Nil t -> case t of
    (TVar _) ->  throwError $ "Error: Cannot infer type of an empty list. Please add a type annotation (e.g. []::Int)"
      
    other -> return (nullSubst, ListT other)

  Yep x -> do
    (s,t) <- infer en x
    return (s, YepT t)

  Nope t -> case t of
    (TVar _) -> throwError $ "Error: Cannot infer the type of Nope. Please add a type annotation, e.g. Nope::Int"

    other -> return (nullSubst, YepT other)

  MchBldr m e -> do
    (s,t) <- infer en e 
    s3'   <- unify (t :->: MachineT) $ getMachineType m
    tv    <- fresh
    return $ (s3', MachineT)


  Pair ex1 ex2 -> do
    (s1, t1) <- infer en ex1
    (s2, t2) <- infer en ex2
    return $ (s1 `compose` s2,PairT t1 t2)

  Unit -> return (nullSubst, UnitT)

  Lit (LitInt _)       -> return $ (nullSubst,IntT)
  Lit (LitBool _)      -> return $ (nullSubst,BoolT)
  Lit (LitPType _)     -> return $ (nullSubst,ProtoT)
  Lit (LitSWMode _)    -> return $ (nullSubst,SWModeT)
  Lit (LitWMode _)     -> return $ (nullSubst,WModeT)
  Lit (LitQString _)   -> return $ (nullSubst,QStringT)
  Lit (LitOptString _) -> return $ (nullSubst,OpStringsT)
  Lit (LitFPath _)     -> return $ (nullSubst,FPathT)
  Lit (LitFType _)     -> return $ (nullSubst,FTypeT)
  Lit (LitDouble _)    -> return $ (nullSubst,DoubleT)
  Lit (LitField _)     -> return $ (nullSubst,FieldT)



lookupEnv :: TypeEnvI -> Sym -> Infer (Subst, Type)
lookupEnv (TypeEnvI env) x =
  case Map.lookup x env of
    Nothing -> throwError $ "Error: Unbound variable " <> (T.pack $ show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (a :->: b) = fv a ++ fv b
    fv  _   = [] -- Probably wrong 

    normtype (a :->: b) = (normtype a) :->: (normtype b)
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
    normtype t  = t 