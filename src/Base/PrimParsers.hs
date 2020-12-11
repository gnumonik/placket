{-# LANGUAGE RankNTypes, OverloadedStrings #-}



module PrimParsers where

import           Classes              (DNSLabel (DNSLabel), DNSName (DNSName),
                                       DNSNameLength (DNSNameLen, DNSPointer),
                                       Flag (Flag), IP4Address (IP4Address),
                                       IP6Address, MacAddr (MacAddr),
                                       MessageContent (MessageContent),
                                       Word24 (..))
import           PrimFuncs            (makeWord16, makeWord32)
import           Text.Megaparsec     
import           Text.Megaparsec.Char     

import           Control.Monad     
import qualified Data.Text as T  
import           Control.Monad.State
import qualified Data.Binary          as B
import qualified Data.Binary.Get      as BG
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BSU
import           Data.Char            
import qualified Data.Vector          as V
import           Data.Word            (Word16, Word32, Word8)
import           Numeric              
import           Text.Show.Functions  ()
import           Data.List (foldl')
import           Text.Hex 


type Parser = Parsec T.Text T.Text 


instance ShowErrorComponent T.Text where
    showErrorComponent x = concatMap (\x -> T.unpack x <> "\n") $ T.lines x

-- To Do: Organize this module

-- toDec from https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell

aWord :: Parser T.Text 
aWord = T.pack <$> manyTill (digitChar <|> letterChar) (char ' ')

filePath :: Parser FilePath 
filePath = lexeme $ try $ do
    path <- quotedString
    return . T.unpack $ path 

int :: Parser Int
int = lexeme $ try $ do
    n' <- (some digitChar ) <?> "Error: Expected an integer, but received a non-digit character."
    let n = read n' :: Int
    return n 



toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0


hex :: Parser String
hex = lexeme $ try $ do
    h <- (some (satisfy (\x -> x `elem` ("abcdef0123456789" :: [Char] )))) <?> "Error parsing hex value: Invalid char. (Valid hex characters are the letters a-f and the numbers 0-9."
    return $!  h

hex0x :: Parser Int
hex0x = lexeme $ try $ do 
    void . string $ "0x"
    h <- hex
    case readHex h of
        [(n,"")] -> return $ n
        _        -> fail $ "Couldn't read a hex value."

bin0b :: Parser Int
bin0b = lexeme $ try $ do
    void $ string "0b"
    b <- some (satisfy $ \x -> x == '0' || x == '1')
    let b' = fromIntegral $ toDec b
    return b' 



parse' :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text T.Text) a
parse' p = parse p ""




parseLex :: Parser b -> Text -> Either Text b
parseLex p txt = case parse' (lexeme $ p) txt of
    Right a -> Right a
    Left err -> Left . T.pack $ errorBundlePretty err 

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    space
    return x

range :: Parser (T.Text,T.Text)
range = lexeme $ try $ do
    first  <- lexeme $ some (satisfy $ \x -> x `notElem` ("- []()~<>,\"" :: [Char]))
    void $ lexeme $  char '-'
    second <- lexeme $ some (satisfy (\x -> x `notElem` ("- []()~<>,\"" :: [Char])))
    return $ (T.pack first,T.pack second)

singleValue :: Parser T.Text
singleValue = lexeme $ try $ do
    val <-  some (satisfy $ \x -> x `notElem` ("- []]()~,<>\"" :: [Char]))
    return $! T.pack val 


atLeastTwo :: Parser [T.Text]
atLeastTwo = lexeme $ try $ do
    void $ char '<'
    rest <- (singleValue `sepBy` (lexeme $ char ',')) 
    void $ char '>'
    return $ rest



just :: Parser a -> Parser (Maybe a)
just p = lexeme $ try $ do
    parsed <- p
    return $! Just parsed 

nothing :: Parser (Maybe a)
nothing = lexeme $ try $ return Nothing 

    
word8 :: Parser Word8
word8 = word8B <|> word8H <|> word8D 
   where 
    word8D :: Parser Word8 
    word8D = lexeme $ try $ do 
        myInt <- some digitChar
        let n = read myInt :: Int
        if n < (fromIntegral $ (minBound :: Word8)) || n > (fromIntegral $ (maxBound :: Word8))
            then fail $ "Error: Value " ++ show myInt ++ " exceeds the bounds of a Word8 (0-255)"
            else return $ (fromIntegral n :: Word8)

    word8B :: Parser Word8 
    word8B = lexeme $ try $ do
        n <- bin0b
        if n < (fromIntegral $ (minBound :: Word8)) || n > (fromIntegral $ (maxBound :: Word8))
            then fail $ "Error: Value " ++ show n ++ " exceeds the bounds of a Word8 (0-255)"
            else return $ (fromIntegral n :: Word8)

    word8H :: Parser Word8
    word8H = lexeme $ try $ do
        n <- hex0x
        if n < (fromIntegral $ (minBound :: Word8)) || n > (fromIntegral $ (maxBound :: Word8))
            then fail $ "Error: Value " ++ show n ++ " exceeds the bounds of a Word8 (0-255)"
            else return $ (fromIntegral n :: Word8)

word16 :: Parser Word16
word16 = word16B <|> word16H <|> word16D
   where
    word16D :: Parser Word16 
    word16D = lexeme $ try $ do
        myInt <- some digitChar
        let n = read myInt :: Int
        if n < (fromIntegral $ (minBound :: Word16)) || n > (fromIntegral $ (maxBound :: Word16))
            then fail $ "Error: Value exceeds the bounds of a Word16 : (" ++ show  (minBound :: Word32) ++ "-" ++ show (maxBound :: Word32) ++ ")"
            else return $ (fromIntegral n :: Word16)
    
    word16B :: Parser Word16 
    word16B = lexeme $ try $ do
        n <- bin0b
        if n < (fromIntegral $ (minBound :: Word16)) || n > (fromIntegral $ (maxBound :: Word16))
            then fail $ "Error: Value exceeds the bounds of a Word16 : (" ++ show  (minBound :: Word32) ++ "-" ++ show (maxBound :: Word32) ++ ")"
            else return $ (fromIntegral n :: Word16)

    word16H :: Parser Word16
    word16H = lexeme $ try $ do
        n <- hex0x
        if n < (fromIntegral $ (minBound :: Word16)) || n > (fromIntegral $ (maxBound :: Word16))
            then fail $ "Error: Value exceeds the bounds of a Word16 : (" ++ show  (minBound :: Word32) ++ "-" ++ show (maxBound :: Word32) ++ ")"
            else return $ (fromIntegral n :: Word16)


word24 :: Parser Word24
word24 = word24B <|> word24H <|> word24D 
   where
    word24D :: Parser Word24
    word24D = lexeme $ try $ do
        myInt <- some digitChar
        let n = read myInt :: Int
        if n < 0 || n > 16777215
            then fail $ "Error: Value exceeds the bounds of a Word24 : (" ++ show  (0 :: Int)  ++ "-" ++ show (16777215 :: Int) ++ ")"
            else return $ (getWord24 n)

    word24B :: Parser Word24
    word24B = lexeme $ try $ do
        n <- bin0b
        if n < 0 || n > 16777215
            then fail $ "Error: Value exceeds the bounds of a Word24 : (" ++ show  (0 :: Int)  ++ "-" ++ show (16777215 :: Int) ++ ")"
            else return $ (getWord24 n)

    word24H :: Parser Word24
    word24H = lexeme $ try $ do
        n <- bin0b
        if n < 0 || n > 16777215
            then fail $ "Error: Value exceeds the bounds of a Word24 : (" ++ show  (0 :: Int)  ++ "-" ++ show (16777215 :: Int) ++ ")"
            else return $ (getWord24 n)

getWord24 :: Int -> Word24
getWord24 w = gW24 $ B.encode (fromIntegral w :: Word32)
  where
      gW24 = BG.runGet go
      go  =  Word24 <$> B.getWord8  <*> B.getWord8 <*> B.getWord8

word32 :: Parser Word32
word32 = word32B <|> word32H <|> word32D
   where 

    word32D = lexeme $ try $ do
        n <- int 
        if n < (fromIntegral $ (minBound :: Word32)) || n > (fromIntegral $ (maxBound :: Word32))
            then fail $
                "Error: Value "
                ++ (show n)
                ++ " exceeds the bounds of a Word32 : ("
                ++ show  (minBound :: Word32)
                ++ "-"
                ++ show (maxBound :: Word32)
                ++ ")"
            else return $  (fromIntegral n :: Word32)

    word32B = lexeme $ try $ do
        n <- bin0b
        if n < (fromIntegral $ (minBound :: Word32)) || n > (fromIntegral $ (maxBound :: Word32))
        then fail $
                "Error: Value "
                ++ show n 
                ++ " exceeds the bounds of a Word32 : ("
                ++ show  (minBound :: Word32)
                ++ "-"
                ++ show (maxBound :: Word32)
                ++ ")"
        else return $  (fromIntegral n :: Word32)

    word32H = lexeme $ try $ do
        n <- hex0x
        if n < (fromIntegral $ (minBound :: Word32)) || n > (fromIntegral $ (maxBound :: Word32))
            then fail $
                "Error: Value "
                ++ show n
                ++ " exceeds the bounds of a Word32 : ("
                ++ show  (minBound :: Word32)
                ++ "-"
                ++ show (maxBound :: Word32)
                ++ ")"
            else return $  (fromIntegral n :: Word32)
    

ip4Addr :: Parser IP4Address
ip4Addr = lexeme $ try $ do
    oct1' <- word8
    _  <- satisfy (== '.')
    oct2' <- word8
    _  <- satisfy (== '.')
    oct3' <- word8
    _  <- satisfy (== '.')
    oct4' <- word8
    return $  IP4Address $ makeWord32  (makeWord16 ( oct1') (oct2')) (makeWord16 ( oct3') ( oct4'))

macAddr :: Parser MacAddr
macAddr = lexeme $ try $ do
    oct1' <- hex
    colon
    oct2' <- hex
    colon
    oct3' <- hex
    colon
    oct4' <- hex
    colon
    oct5' <- hex
    colon
    oct6' <- hex
    let octs = map ((\x -> fromIntegral x :: Word8) . fst) $ concatMap readHex [oct1',oct2',oct3',oct4',oct5',oct6']
    case octs of
        [a,b,c,d,e,f] -> return $  MacAddr a b c d e f
        _             -> fail $ "Error: Improperly formatted MAC Address"
  where
      colon :: Parser ()
      colon = lexeme $ void (char ':')



ip6Addr :: Parser IP6Address
ip6Addr = undefined -- DO LATER

--make them enclode byte-string-ey things in quotes
byteString :: Parser BS.ByteString
byteString = hexBS <|> asciiBS 
 where 
    hexBS :: Parser ByteString
    hexBS = lexeme $ try $ do
        h <- hex
        case decodeHex (T.pack h) of
            Just hBs -> return hBs
            Nothing  -> fail "Error! Could not read string of chars as a hexadecimal bytestring. (Perhaps you entered an odd number of characters, or an invalid character?)"

    asciiBS :: Parser ByteString
    asciiBS = lexeme $ try $ do
        aString <- quotedString 
        return $  BSU.fromString (T.unpack aString)
flag :: Parser Flag
flag = flagBool <|> flagNum 
   where 
    flagNum :: Parser Flag
    flagNum = lexeme $ try $ do
        f <- satisfy (\x -> x == '0' || x == '1')
        case f of
            '0' -> return . Flag $ False
            '1' -> return . Flag $ True 
        
    flagBool :: Parser Flag
    flagBool = lexeme $ try $ do
            myBool <- bool
            return $ Flag myBool
    
    bool :: Parser Bool
    bool = lexeme $ try $ do
            myChar <- letterChar
            case toUpper myChar of
                'T' -> return True
                'F' -> return False
                _   -> fail $ "Error: Cannot parse char \'" ++  [myChar] ++ "\' as a flag. Use T to indicate that a flag is set and F to indicate that it is not, e.g.: myFlag=T or myFlag=F"

-- Broken as hell
dnsName :: Parser DNSName
dnsName = lexeme $ try $ do
    void $ char '"'
    myDNSName <- some (dnspointer <|> dnsnamelabel) 

    return $ DNSName $ V.fromList myDNSName
  where
      dnspointer = lexeme $ try $ do
          void $ lexeme $ string $ "POINTER:"
          n <- word16
          return $ (DNSPointer n, DNSLabel BS.empty)

      dnsnamelabel = lexeme $ try $ do
          rawStr <- manyTill anySingle  (      void (char '.') 
                                              <|> void (char '@') 
                                              <|> void (char '\"')
                                              <|> (void $ char '\"')) 
          let bStr = BSU.fromString rawStr
          let bsLen = fromIntegral (BS.length bStr) :: Word8
          return $ (DNSNameLen bsLen,DNSLabel bStr)

messageContent :: Parser MessageContent
messageContent = lexeme $ try $ do
    bs <- byteString 
    return $ MessageContent $  bs




voidCh :: Parser Char -> Parser ()
voidCh p = lexeme $ try $ do
    void p

alwaysFail :: forall a. Parser a
alwaysFail = lexeme $ try $ do
    fail ""


quotedString :: Parser T.Text
quotedString = lexeme $ try $ do
    (void $ char '"') <?> "Error: Expecting a string enclosed by quotes, but there is no initial quotation mark."
    body <- manyTill (satisfy $ \x -> x /= '"') (lookAhead $ char '"')
    (void $ char '"') <?> "Error: Expecting a string enclosed by quotes, but there is no ending quotation mark."
    return $! T.pack body 