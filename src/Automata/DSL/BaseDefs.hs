{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}

module BaseDefs where

import Syntax
import qualified Data.Text as T 


-- Composite Types 
fSelT :: Type
fSelT = PairT BinaryOpT (PairT OpStringsT FieldT)

fSelPlusT :: Type
fSelPlusT = PairT BinaryOpT (PairT OpStringsT OpStringsT)

fSelExpT :: Type
fSelExpT = PredT fSelT 

pSelT :: Type
pSelT = PairT ProtoT fSelExpT 

pSelExpT :: Type 
pSelExpT = PredT pSelT 

mSelT :: Type 
mSelT = YepT pSelT 

mSelExpT :: Type
mSelExpT = ListT pSelT 

fBuilderT :: Type
fBuilderT = PairT OpStringsT FieldT 

fBuilderExpT :: Type 
fBuilderExpT = YepT (ListT fBuilderT)

pBuilderT :: Type
pBuilderT = PairT ProtoT fBuilderExpT 


unDef :: Def -> Expr
unDef (Def _ _ x) = x 




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


mkSelect :: Def 
mkSelect = Def "select" ["pSel"] $ 
              Lam "pSel" pSelT  
               (MchBldr MK_SELECT (Var "pSel"))

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
