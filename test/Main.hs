module Main where 

import PrimParserSpec 
import SerializerSpec 
import RecordParserSpec 
import MachineParserSpec
import SourceParserSpec 

main = do 
  ppSpec
  testRecordParsers
  testMachineParsers
  testSourceParsers
  testCommandParsers