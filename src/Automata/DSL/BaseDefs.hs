{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}

module BaseDefs where

import Syntax
import Data.Map (Map)
import qualified Data.Map as Map 
import qualified Data.Text as T 
import Control.Monad.State.Strict

initEnv :: DSLEnv 
initEnv = 
  let myTypes = (Map.fromList $ baseTypes <> (Map.toList compoundTypes))
      myEnv = DSLEnv (TypeEnv myTypes)  defMap
  in myEnv 

initDSL :: DSLMonad ()
initDSL = lift . put $ initEnv


-- Composite Types

unSyn :: Type -> Type
unSyn (Type _ t) = t 
unSyn other      = other 

casesT :: Type
casesT = ListT (PairT pSelExpT MachineT )

compoundTypes :: Map T.Text Type
compoundTypes = Map.fromList . map go 
      $ [fSelT
        ,fSelPlusT 
        ,fSelExpT
        ,pSelT
        ,pSelExpT
        ,mSelT 
        ,mSelExpT
        ,fBuilderT
        ,fBuilderExpT 
        ,pBuilderT]
  where
    go (Type nm ex) = (nm,ex)


baseTypes :: [(T.Text, Type)]
baseTypes = [ ("Bool",BoolT)
        , ("Int",IntT)
        , ("PType",ProtoT)
        , ("PMode", PModeT)
        , ("WMode",WModeT) 
        , ("QString", QStringT)
        , ("OpStrings",OpStringsT)
        , ("SWMode",SWModeT)
        , ("FPath",FPathT)
        , ("FType",FTypeT)
        , ("Machine",MachineT)
        , ("Double",DoubleT)
        , ("Source", SourceT)
        , ("Field", FieldT)
        , ("()"   , UnitT)]

fSelT :: Type
fSelT = Type "FieldSelector" $ PairT BinaryOpT (PairT OpStringsT FieldT)

fSelPlusT :: Type
fSelPlusT = Type "FieldReference" $ PairT BinaryOpT (PairT OpStringsT OpStringsT)

fSelExpT :: Type
fSelExpT = Type "Predicate FieldSelector" $ PredT fSelT 

pSelT :: Type
pSelT = Type "ProtocolSelector" $ PairT ProtoT fSelExpT 

pSelExpT :: Type 
pSelExpT = Type "Predicate ProtocolSelector" $ PredT pSelT 

mSelT :: Type 
mSelT = Type "ProtocolSelector" $ YepT pSelT 

mSelExpT :: Type
mSelExpT = Type "[ProtocolSelector]" $ ListT pSelT 

fBuilderT :: Type
fBuilderT = Type "FieldBuilder" $ PairT OpStringsT FieldT 

fBuilderExpT :: Type 
fBuilderExpT = Type "FieldBuilders" $ YepT (ListT fBuilderT)

pBuilderT :: Type
pBuilderT = Type "ProtocolBuilder" $ PairT ProtoT fBuilderExpT 

-- Utility function

unDef :: Def -> Expr
unDef (Def _ _ x) = x 

-- Base Defs

defToMap :: [Def] -> Map T.Text Expr 
defToMap d = Map.fromList $ map go d
  where
    go (Def n _ e) = (n,e)

defMap :: Map T.Text Expr
defMap = defToMap baseDefs 

baseDefs :: [Def]
baseDefs = [mkPPrint 
         , mkPrintField
         , mkWriteField
         , mkPop
         , mkPull 
         , mkExtract 
         , mkCut 
         , mkPush 
         , mkLift 
         , mkSet 
         , mkChecksum 
         , mkModifyOpt 
         , mkInsertOpt 
         , mkDeleteOpt
         , mkSelect
         , mkDiscard
         , mkAlert
         , mkVoid
         , mkReport
         , mkCreate
         , mkCount
         , mkBuffer 
         , mkUntil
         , mkUnless
         , mkWhen
         , mkAfter 
         , mkSwitch 
         , mkTimeSwitch
         , mkCase]


mkProtoBuilder :: Expr
mkProtoBuilder  = Lam "tstring" (ProtoT) 
                      $ Lam "fbuilders" (ListT fBuilderT)
                         $ Pair (Var "tstring") (Var "fbuilders")

mkFieldBuilders :: Expr
mkFieldBuilders = Lam "x" fBuilderT
                    $ Lam "y" (ListT fBuilderT) 
                      $ Cons (Var "x") (Var "y")

mkFieldBuilder :: Expr 
mkFieldBuilder = Lam "ostrs" OpStringsT 
                  $ Lam "field" (PairT OpStringsT FieldT) 
                        (Pair (Var "ostrs") (Var "field")) 




mkPPrint :: Def 
mkPPrint = 
  Def "prettyPrint" ["pMode"] $ 
    Lam "pMode" PModeT $ MchBldr MK_PRETTYPRINT (Var "pMode")

mkPrintField :: Def
mkPrintField = 
  Def "printField" ["qString","wMode"]$
    Lam "qString" QStringT $ 
      Lam "wMode" WModeT $ 
        MchBldr MK_PRINTFIELD (Pair (Var "qString") (Var "wMode")) 


mkWriteField :: Def 
mkWriteField = 
  Def "writeField" ["path","label","pMode","pType","oStrs"] $
    Lam "path" FPathT $ 
      Lam "label" QStringT $ 
        Lam "pMode" PModeT $ 
          Lam "pType" ProtoT $ 
            Lam "oStrs" OpStringsT $
              MchBldr MK_WRITEFIELD $ 
                  Pair (Var "path") $ 
                      Pair  (Var "label") $
                        Pair  (Var "pMode") $
                          Pair (Var "pType") (Var "oStrs")


mkPop :: Def 
mkPop = Def "pop" ["pType"] 
          $ Lam "pType" ProtoT (MchBldr MK_POP (Var "pType"))

mkPull :: Def 
mkPull = Def "pull" ["pType"] 
          $ Lam "pType" ProtoT (MchBldr MK_PULL (Var "pType"))

mkExtract :: Def 
mkExtract = Def "extract" ["pType"] 
          $ Lam "pType" ProtoT (MchBldr MK_EXTRACT (Var "pType"))

mkCut :: Def 
mkCut = Def "cut" ["pType"] 
          $ Lam "pType" ProtoT (MchBldr MK_CUT (Var "pType"))

mkPush :: Def 
mkPush = Def "push" ["pType"] 
          $ Lam "pBuilder" pBuilderT (MchBldr MK_PUSH (Var "pType"))

mkLift :: Def 
mkLift = Def "lift" ["pType"] 
          $ Lam "pBuilder" pBuilderT (MchBldr MK_LIFT (Var "pType"))

mkSet :: Def 
mkSet = Def "set" ["pBuilder"]
          $ Lam "pBuilder" pBuilderT (MchBldr MK_SET (Var "pBuilder"))

mkChecksum :: Def 
mkChecksum = Def "checksum" [] 
          $ Lam "x" UnitT $ MchBldr MK_CHECKSUM Unit

mkModifyOpt :: Def
mkModifyOpt = 
  Def "modifyOpt" ["pType","fType","fSelExp","fBuilder"]
    $ Lam "pType" ProtoT 
    $ Lam "fType" FTypeT
    $ Lam "fSelExp" fSelExpT 
    $ Lam "fBuilder" fBuilderT
      $  MchBldr MK_MODIFYOPT
        $ Pair  (Var "pType")
            (Pair  (Var "fType")
              (Pair  (Var "fSelExp") 
                 (Var "fBuilder") ))
mkInsertOpt :: Def
mkInsertOpt = 
  let p  = "p"
      f  = "f"
      fs = "fs"
  in Def "insertOpt" [p,f,fs]
       $ Lam p ProtoT
         $ Lam f FTypeT
           $ Lam fs (ListT fBuilderT)
              $ MchBldr MK_INSERTOPT
                $ Pair (Var p) 
                  $ Pair (Var f) (Var fs)

mkDeleteOpt :: Def
mkDeleteOpt =
  let p  = "p"
      f  = "f"
      fs = "fs"
  in Def "deleteOpt" [p,f]
      $ Lam p ProtoT
        $ Lam f FTypeT
          $ Lam fs fSelExpT
            $ MchBldr MK_DELETEOPT (Pair (Var p) (Pair (Var f) (Var fs)))


mkSelect :: Def 
mkSelect = Def "select" ["pSel"] $ 
              Lam "pSel" pSelT  
               (MchBldr MK_SELECT (Var "pSel"))

mkDiscard :: Def 
mkDiscard = Def "discard" ["pSel"] $ 
              Lam "pSel" pSelT  
               (MchBldr MK_DISCARD (Var "pSel"))

mkAlert :: Def
mkAlert = 
  let q = "qString"
      p = "pSel"
  in Def "alert" [q,p] $ 
      Lam q QStringT $
        Lam p pSelT $ 
          (MchBldr MK_ALERT $ Pair (Var q) (Var p))

mkVoid :: Def 
mkVoid = Def "void" [] $ MchBldr MK_VOID Unit 

mkReport :: Def
mkReport 
  = let q = "q"
    in Def "report" [q]
        $ Lam q QStringT 
          $ MchBldr MK_REPORT (Var q)

mkCreate :: Def
mkCreate = 
  let w = "w"
      r = "d"
      b = "b"
  in 
    Def "create" [w,r,b] $  
    Lam w IntT 
      $ Lam r IntT 
        $ Lam b (ListT pBuilderT)
          $ MchBldr MK_CREATE
            $ Pair (Var w)
              $ Pair (Var r) (Var b)

mkCount :: Def
mkCount = 
  let i = "i"
  in Def "count" [i] 
      $ Lam i IntT 
        $ MchBldr MK_COUNT (Var i)

mkBuffer :: Def
mkBuffer =
  let i = "i"
  in Def "buffer" [i]
      $ Lam i IntT 
        $ MchBldr MK_BUFFER (Var i)

mkDump :: Def
mkDump = 
  let f = "f"
      i = "i"
  in Def "dump" [f,i]
      $ Lam "f" FPathT
        $ Lam i IntT 
          $ MchBldr MK_DUMP (Pair (Var f) (Var i))

mkUntil :: Def
mkUntil = 
  let p = "p"
      m = "m"
  in 
    Def "until" [p,m]
      $ Lam p pSelExpT 
        $ Lam m MachineT
          $ MchBldr MK_UNTIL (Pair (Var p) (Var m))

mkUnless :: Def
mkUnless = 
  let p = "p"
      m = "m"
  in 
    Def "unless" [p,m]
      $ Lam p pSelExpT 
        $ Lam m MachineT
          $ MchBldr MK_UNLESS (Pair (Var p) (Var m))

mkWhen :: Def
mkWhen = 
  let p = "p"
      m = "m"
  in 
    Def "when" [p,m]
      $ Lam p pSelExpT 
        $ Lam m MachineT
          $ MchBldr MK_WHEN (Pair (Var p) (Var m))


mkAfter :: Def
mkAfter = 
  let p = "p"
      m = "m"
  in 
    Def "after" [p,m]
      $ Lam p pSelExpT 
        $ Lam m MachineT
          $ MchBldr MK_AFTER(Pair (Var p) (Var m))

mkSwitch :: Def
mkSwitch = 
  let m = "m"
      p = "p"
      mch1 = "mch1"
      mch2 = "mch2"
  in 
    Def "switch" [m,p,mch1,mch2]
      $ Lam m SWModeT
        $ Lam p PModeT 
          $ Lam mch1 MachineT
            $ Lam mch2 MachineT
              $ MchBldr MK_SWITCH 
                $ Pair (Var m)
                  $ Pair (Var p)
                    $ Pair (Var mch1) (Var mch2)

mkTimeSwitch :: Def
mkTimeSwitch = 
  let t  = "t"
      m1 = "m1 " 
      m2 = "m2"
  in Def "timeSwitch" [t,m1,m2]
      $ Lam t IntT 
        $ Lam m1 MachineT
          $ Lam m2 MachineT
            $ MchBldr MK_TIMESWITCH 
              $ Pair (Var t)
                $ Pair (Var m1) (Var m2)

mkCase :: Def
mkCase = 
  let cs = "cs"
  in Def "case" [cs]
       $ Lam cs casesT 
         $ MchBldr MK_CASE (Var cs)


{--
mkListenFor :: Def
mkListenFor = 
  let mplus = "m"
      d     = "d"
      maxTO = "t"
      mult  = "mult"
      mArg  = "mch"
  in Def "listenFor" [mplus,d,maxTO,mult,mArg]
        $ Lam mplus msgSel
--} 
-- Figure out how to represent msgSel+ later 