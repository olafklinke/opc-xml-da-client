{-| Abstraction of the parsing capabilites 
needed for OpcXmlDaClient.Protocol.XmlParsing -}
{-# LANGUAGE TypeSynonymInstances #-}
module OpcXmlDaClient.XmlSchemaValues.ContentParser where
import OpcXmlDaClient.Base.Prelude hiding (Read)
import qualified Attoparsec.Data as AttoparsecData
import qualified Data.Attoparsec.Text as Atto
import qualified OpcXmlDaClient.XmlSchemaValues.Attoparsec
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day,DiffTime,NominalDiffTime,UTCTime,TimeZone,TimeOfDay)
import Data.Scientific (Scientific)
import Data.UUID (UUID)
import qualified OpcXmlDaClient.XmlSchemaValues.Types as XmlSchema
import XmlParser (attoparsedContent,textContent,Content)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Char (ord,toLower,isDigit)

-- * Parsing Xml content 

-- | This class is the union of the lenient parsers in 
-- 'AttoparsecData' and the parsers defined in 'OpcXmlDaClient.XmlSchemaValues.Attoparsec'. 
class (Monad p, Alternative p) => XmlContentParser p where
    parsedContent :: p a -> Content a -- ^ parse in Xml context
    decimal :: Integral a => p a
    parseXmlBool :: p Bool
    parseXmlChar :: p Char
    parseXmlDouble :: p Double
    parseXmlInt :: p Int
    parseXmlInt8 :: p Int8
    parseXmlInt16 :: p Int16
    parseXmlInt32 :: p Int32
    parseXmlInt64 :: p Int64
    parseXmlInteger :: p Integer
    parseXmlWord :: p Word
    parseXmlWord8 :: p Word8
    parseXmlWord16 :: p Word16
    parseXmlWord32 :: p Word32
    parseXmlWord64 :: p Word64
    parseXmlByteString :: p ByteString
    parseXmlText :: p Text
    parseXmlString :: p String
    parseXmlDay :: p Day
    parseXmlDiffTime :: p DiffTime
    parseXmlNominalDiffTime :: p NominalDiffTime
    parseXmlUTCTime :: p UTCTime
    parseXmlTimeZone :: p TimeZone
    parseXmlTimeOfDay :: p TimeOfDay
    parseXmlScientific :: p Scientific
    parseXmlUUID :: p UUID
    parseXmlTime :: p XmlSchema.Time
    parseXmlDate :: p XmlSchema.Date
    parseXmlDuration :: p XmlSchema.Duration

instance XmlContentParser Atto.Parser where
    parsedContent = attoparsedContent
    decimal = Atto.decimal 
    parseXmlBool = AttoparsecData.lenientParser
    parseXmlChar = AttoparsecData.lenientParser
    parseXmlDouble = AttoparsecData.lenientParser
    parseXmlInt = AttoparsecData.lenientParser
    parseXmlInt8 = AttoparsecData.lenientParser
    parseXmlInt16 = AttoparsecData.lenientParser
    parseXmlInt32 = AttoparsecData.lenientParser
    parseXmlInt64 = AttoparsecData.lenientParser
    parseXmlInteger = AttoparsecData.lenientParser
    parseXmlWord = AttoparsecData.lenientParser
    parseXmlWord8 = AttoparsecData.lenientParser
    parseXmlWord16 = AttoparsecData.lenientParser
    parseXmlWord32 = AttoparsecData.lenientParser
    parseXmlWord64 = AttoparsecData.lenientParser
    parseXmlByteString = AttoparsecData.lenientParser
    parseXmlText = AttoparsecData.lenientParser
    parseXmlString = AttoparsecData.lenientParser
    parseXmlDay = AttoparsecData.lenientParser
    parseXmlDiffTime = AttoparsecData.lenientParser
    parseXmlNominalDiffTime = AttoparsecData.lenientParser
    parseXmlUTCTime = AttoparsecData.lenientParser
    parseXmlTimeZone = AttoparsecData.lenientParser
    parseXmlTimeOfDay = AttoparsecData.lenientParser
    parseXmlScientific = AttoparsecData.lenientParser
    parseXmlUUID = AttoparsecData.lenientParser
    parseXmlTime = OpcXmlDaClient.XmlSchemaValues.Attoparsec.time
    parseXmlDate = OpcXmlDaClient.XmlSchemaValues.Attoparsec.date
    parseXmlDuration = OpcXmlDaClient.XmlSchemaValues.Attoparsec.duration

-- * A dead-simple text parser

type SimpleParser = StateT Text Maybe

-- This instance is incomplete yet. 
instance XmlContentParser SimpleParser where
    parsedContent p = textContent >>= \s -> case runStateT p s of
        Nothing    -> fail "XmlContentParser fail"
        Just (a,_) -> return a -- Atto.parseOnly used in attoparsedContent also discards the unparsed remainder
    decimal = let step a c = a * 10 + fromIntegral (ord c - 48) in StateT (\s -> 
        let (n,s') = T.span isDigit s in if T.null n
            then Nothing
            else Just (T.foldl' step 0 n,s'))
    parseXmlBool = StateT $ \s -> do
        (c,cs) <- T.uncons s
        case toLower c of
            -- as in Attoparsec.Data.Explicit
            '0' -> Just (False,cs)
            '1' -> Just (True,cs)
            'f' -> (("alse" `parsedAs` False) cs) <|> Just (False,cs)
            't' -> (("rue" `parsedAs` True) cs) <|> Just (True,cs)
            'n' -> (("o" `parsedAs` False) cs) <|> Just (False,cs)
            'y' -> (("es" `parsedAs` True) cs) <|> Just (True,cs)
            _ -> mzero
    parseXmlChar = StateT T.uncons
    parseXmlText = StateT (\s -> Just (s,mempty))
    parseXmlString = StateT (\s -> Just (T.unpack s,mempty))
    parseXmlWord = decimal
    parseXmlWord8 = decimal
    parseXmlWord16 = decimal
    parseXmlWord32 = decimal
    parseXmlWord64 = decimal
    parseXmlInt = signed decimal
    parseXmlInt8 = signed decimal
    parseXmlInt16 = signed decimal
    parseXmlInt32 = signed decimal
    parseXmlInt64 = signed decimal
    parseXmlInteger = signed decimal

-- | convenient wrapper around 'T.stripPrefix'
parsedAs :: Text -> a -> Text -> Maybe (a,Text)
parsedAs pfx a input = fmap ((,) a) (T.stripPrefix pfx input)

-- | optional preceding sign
signed :: Num a => SimpleParser a -> SimpleParser a
signed p = StateT $ \s -> do
    (c,cs) <- T.uncons s
    case c of
        '-' -> runStateT (fmap negate p) cs
        '+' -> runStateT p cs
        _ -> runStateT p s
