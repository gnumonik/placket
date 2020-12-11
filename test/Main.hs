module Main where 

import PrimParserSpec ( ppSpec ) 
import RecordParserSpec ( testRecordParsers ) 
import MachineParserSpec ( testMachineParsers )
import SourceParserSpec ( testSourceParsers, testCommandParsers ) 


-- I'm not entirely sure why, but `stack test` won't actually run all of these at once.
-- Uncomment the machine and source parsers and comment the rest to test those.


main = do 
  --testMachineParsers
  --testSourceParsers
  testCommandParsers
  ppSpec
  testRecordParsers
