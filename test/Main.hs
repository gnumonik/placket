module Main where 

import PrimParserSpec 
import SerializerSpec 
import RecordParserSpec 
import MachineParserSpec


main = do 
  ppSpec
  testRecordParsers
  testMachineParsers 