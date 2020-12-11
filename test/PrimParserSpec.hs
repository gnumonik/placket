{-# LANGUAGE OverloadedStrings #-}

module PrimParserSpec where

import Test.Hspec
import Classes 
import Control.Applicative hiding (some)
import qualified Data.Text as T 
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Data.Maybe
import Text.Megaparsec.Char
import PrimParsers 
import Data.Word ( Word8, Word16, Word32 )

parseT :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text T.Text) a
parseT p txt = parse' (lexeme $ p) txt

ppSpec :: IO ()
ppSpec = hspec $
    describe "PrimParsers" $ do 

      describe "filePath" $ do
        it "returns correct result" $
          parseT filePath "\"someString\"" `shouldParse`   "someString"

      describe "int" $ do
        it "returns correct result" $
          parseT int "33" `shouldParse` (33 :: Int)

      describe "hex" $ do
        it "returns correct result" $
          parseT hex "face666" `shouldParse` "face666"

      describe "hex0x" $ do
        it "returns correct result" $
          parseT hex0x "0xface666" `shouldParse` 262989414

      describe "bin0b" $ do
        it "returns correct result" $ 
          parseT bin0b "0b1010011010" `shouldParse` 666

      describe "range" $ do 
        it "returns correct result" $ 
          parseT range "1-10" `shouldParse` ("1","10")

      describe "singleValue" $ do
        it "returns correct result" $ 
          parseT singleValue "ab:cd:ef:12:34" `shouldParse` "ab:cd:ef:12:34"

      describe "atLeastTwo" $ do
        it "returns correct result" $ 
          parseT atLeastTwo "<101, 666, abcde>" `shouldParse` ["101","666","abcde"]

      describe "word8(Dec)" $ do
        it "returns correct result" $ 
          parseT word8 "101" `shouldParse` (101 :: Word8)

      describe "word8(Bin)" $ do
        it "returns correct result" $ 
          parseT word8 "0b11001100" `shouldParse` (204 :: Word8)

      describe "word8(Hex)" $ do
        it "returns correct result" $ 
          parseT word8 "0xab" `shouldParse` (171 :: Word8)

      describe "word16(Dec)" $ do
        it "returns correct result" $ 
          parseT word16 "101" `shouldParse` (101 :: Word16)

      describe "word16(Bin)" $ do
        it "returns correct result" $ 
          parseT word16 "0b11001100" `shouldParse` (204 :: Word16)

      describe "word16(Hex)" $ do
        it "returns correct result" $ 
          parseT word16 "0xab" `shouldParse` (171 :: Word16)

-- ignoring Word24 for now

      describe "word32(Dec)" $ do
        it "returns correct result" $ 
          parseT word32 "101" `shouldParse` (101 :: Word32)

      describe "word32(Bin)" $ do
        it "returns correct result" $ 
          parseT word32 "0b11001100" `shouldParse` (204 :: Word32)

      describe "word32(Hex)" $ do
        it "returns correct result" $ 
          parseT word32 "0xab" `shouldParse` (171 :: Word32)

      describe "ip4Addr" $ do
        it "returns correct result" $ 
          parseT ip4Addr "192.168.0.1" `shouldParse` (IP4Address 3232235521)

      describe "macAddr" $ do
        it "returns correct result" $
          parseT macAddr "ff:ff:ff:ff:ff:ff" `shouldParse` (MacAddr 255 255 255 255 255 255)

-- Add IP6Addr when implemented 

-- dunno how to write the tests for bytestring parsers. look into it later
      describe "flag(TF)" $ do
        it "returns correct result" $ 
          parseT flag "T" `shouldParse` (Flag True)

      describe "flag(01)" $ do
        it "returns correct result" $ 
          parseT flag "0" `shouldParse` (Flag False)

--dnsName is broken as hell
      describe "quotedString" $ do 
        it "returns correct result" $ 
          parseT quotedString "\"of thee i string\"" `shouldParse` "of thee i string"
          
      return () 