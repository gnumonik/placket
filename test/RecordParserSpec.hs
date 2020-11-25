{-# LANGUAGE OverloadedStrings #-}

module RecordParserSpec where

import RecordParsers
import RecordTypes 
import FieldClasses 
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import PrimParsers 
import PrimParserSpec
import qualified Data.Text as T 

doop :: Parser T.Text 
doop = lexeme $ string "doop"

testRecordParsers :: IO ()
testRecordParsers = hspec $ 
  describe "RecordParsers" $ do

    describe "ProtocolType (lower)" $ do
      it "returns correct result" $ 
        parseT protocolType "eth" `shouldParse` "ETH"

    describe "ProtocolType (upper)" $ do
      it "returns correct result" $ 
        parseT protocolType "DNS" `shouldParse` "DNS"

    describe "predicate(Atom)" $ do 
      it "returns correct result" $ 
        parseT (predicate doop) "doop" `shouldParse` ATOM "doop"

    describe "predicate(Not)" $ do
      it "returns correct result" $ 
        parseT (predicate doop) "not doop" `shouldParse` NOT (ATOM "doop") 
    
    describe "predicate(And1)" $ do 
      it "returns correct result" $ 
        parseT (predicate doop) "doop && not doop" `shouldParse` (ATOM "doop" :&&: NOT (ATOM "doop"))

    describe "predicate(And2)" $ do
      it "returns correct result" $ 
        parseT (predicate doop) "(doop && doop) && doop" `shouldParse` ((ATOM "doop" :&&: ATOM "doop") :&&: ATOM "doop")

    describe "predicate(Or1)" $ do
      it "returns correc result" $ 
        parseT (predicate doop) "doop || not doop"  `shouldParse` (ATOM "doop" :||: NOT (ATOM "doop"))

    describe "predicate(Or2)" $ do 
      it "returns correct result" $ 
        parseT (predicate doop) "not doop || (doop || doop)" `shouldParse` (NOT (ATOM "doop") :||: (ATOM "doop" :||: ATOM "doop"))

    describe "nonContigSet" $ do
      it "returns correct result" $ 
        parseT nonContigSet "[1 10]" `shouldParse` (NonContigSet ["1" , "10"])

    describe "fieldRange" $ do
      it "returns correct result" $ 
        parseT fieldRange "1-10" `shouldParse` RangeOfVals "1" "10"

    describe "fieldSingVal" $ do
      it "returns correct result" $ 
        parseT fieldSingVal "222.2" `shouldParse` SingleValue "222.2"

    describe "fieldBuilder" $ do
        it "returns correct result" $ 
          parseT fieldBuilder "shoop.doop=666" `shouldParse` FieldBuilder ["shoop","doop"] (SingleValue "666")

    describe "fieldBuilderExp" $ do 
        it "returns correct result" $ 
          parseT fieldBuilderExp "(shoop.doop=69 flop.drop=1-10 scewp.pewp=[3 4])" 
            `shouldParse` FieldBuilderExp [
                                          FieldBuilder ["shoop","doop"] (SingleValue "69")
                                        , FieldBuilder ["flop","drop"] (RangeOfVals "1" "10")
                                        , FieldBuilder ["scewp","pewp"] (NonContigSet ["3","4"])
                                          ]  

    describe "fieldBuilderExpDefaults" $ do 
        it "returns correct result" $ 
          parseT fieldBuilderExp "" `shouldParse` AllDefaults 

    describe "fieldBuilderExpDefaults2" $ do 
        it "returns correct result" $ 
          parseT fieldBuilderExp "plop" `shouldParse` AllDefaults    

    describe "protocolBuilder" $ do 
      it "returns correct result" $ 
        parseT protocolBuilder "eth (etherType=666 src=aa:aa:aa:aa:aa:aa)" 
          `shouldParse`
              ProtocolBuilder "ETH" 
                (
                  FieldBuilderExp 
                      [
                        FieldBuilder ["etherType"] (SingleValue "666")
                       ,FieldBuilder ["src"]  (SingleValue "aa:aa:aa:aa:aa:aa")
                        
                      ]
                      )

    describe "comp (all at once)" $ do
      it "returns correct result" $ 
        parseT (some comp) "= /= != > >= < <=" `shouldParse` [EQ',NOTEQ',NOTEQ',GT',GTE',LT',LTE']

    describe "varExpr" $ do
      it "returns correct result" $ 
        parseT varExpr "$(flags.df)" `shouldParse` RefVarExpr ["flags","df"] Nothing 

    describe "fieldSelector" $ do
      it "returns correct result" $ 
        parseT fieldSelector "flags.df!=T" `shouldParse` FieldSelector ["flags","df"] NOTEQ' (Literal . SingleValue $ "T")

    describe "fieldSelectorWC" $ do
      it "returns correct result" $ 
        parseT fieldSelector "flags.df=*" `shouldParse` FieldSelector ["flags","df"] EQ' CompareToWC

    describe "fieldSelectorPlus" $ do 
      it "returns correct result" $ 
        parseT fieldSelectorPlus "flags.df=$(flags.df)" `shouldParse` FieldSelector ["flags","df"] EQ' (RefVarExpr ["flags","df"] Nothing)

    describe "fieldSelectorExp" $ do
      it "returns correct result" $ 
        parseT fieldSelectorExp "( flags.df=T || proto<66 ; not checksum=99 && checksum >=77)" `shouldParse`  
        
          FieldSelectorExp 
        ( 
          
          (
            ATOM  (FieldSelector ["flags","df"] EQ' (Literal . SingleValue $ "T")) 
            :||: 
            ATOM (FieldSelector ["proto"] LT' (Literal . SingleValue $ "66"))
          )      
          :&&: 
          (
            NOT (ATOM (FieldSelector ["checksum"] EQ' (Literal . SingleValue $ "99"))) 
            :&&: 
            ATOM (FieldSelector ["checksum"] GTE' (Literal . SingleValue $ "77")) 
          )
        )

    -- assuming fieldSelectorExpPlus works if the last two do. cannot. write. out. an. example. that. long. 

    describe "protocolSelector" $ do
      it "returns correct result" $ 
        parseT protocolSelector "ETH (etherType=666)" 
          `shouldParse` 
            ProtocolSelector "ETH" (FieldSelectorExp . ATOM $ FieldSelector ["etherType"] EQ' (Literal . SingleValue $ "666"))  

  -- if the other tests pass then so will protocolSelectorPlus 

    describe "msgSelector" $ do 
      it "returns correct result" $ 
        parseT  msgSelector "ETH (etherType=666)" 
         `shouldParse`
            MsgSelector  
              (
                ProtocolSelector  "ETH" 
                  (
                    FieldSelectorExp . ATOM $ 
                      FieldSelector ["etherType"] EQ' (Literal . SingleValue $ "666")
                  )  
              )

    describe "msgSelectorExp" $ do 
        it "returns  correct result" $ 
          parseT msgSelectorExp "[ ARP ; ETH (etherType=2054) ]" 
            `shouldParse` 
                MsgSelectorExp 
                  [
                    MsgSelector  $  ProtocolSelector  "ARP" FieldSelectorExpWC 
                  , 
                    MsgSelector $ ProtocolSelector  "ETH" (FieldSelectorExp . ATOM $ FieldSelector ["etherType"] EQ' (Literal . SingleValue $ "2054"))]
                         
    