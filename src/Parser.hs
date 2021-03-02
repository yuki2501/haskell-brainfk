{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Parser(BrainFk(BRight,BLeft,Increment,Decrement,BPrint,BGet,BLoop,BNil,BError,BInvalidChar),parseBrainFk) where
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Data.Word

data BrainFk a  where
  BRight ::   BrainFk () 
  BLeft  ::   BrainFk ()
  Increment ::  BrainFk ()
  Decrement ::   BrainFk ()
  BPrint :: BrainFk ()
  BGet ::    BrainFk ()
  BLoop ::  Text -> BrainFk () 
  BInvalidChar ::  BrainFk ()
  BNil ::  BrainFk ()
  BError :: BrainFk ()
instance Show (BrainFk a) where
  show (BRight ) = "BRight " 
  show (BLeft ) = "BLeft " 
  show (Increment ) = "Increment " 
  show (Decrement ) = "Decrement" 
  show (BPrint ) = "BPrint " 
  show (BGet ) = "BGet " 
  show (BLoop r ) = "BLoop " 
  show BNil = "BNil" 
  show (BInvalidChar ) = "BInvaildChar " 
  show BError = "BError"
parseBrainFk ::   Text -> [BrainFk ()]
parseBrainFk s = let result = showParseResult $ parse (brainfkParser <* endOfInput) s `feed` pack ""
                  in case result of
                       Right x -> x
                       Left _ -> [BError]


brainfkParser :: Parser [BrainFk() ]
brainfkParser =  many1 brainfk
  where
    brainfk :: Parser (BrainFk() )
    brainfk = bright <|> bleft <|> increment <|> decrement <|> bprint <|> bget<|> bloop <|> binvaildchar
    bright :: Parser (BrainFk ())
    bright = BRight <$ char '>' 
    bleft :: Parser (BrainFk ())
    bleft = BLeft <$ char '<' 
    increment :: Parser (BrainFk ())
    increment = Increment <$ char '+' 
    decrement :: Parser (BrainFk ())
    decrement = Decrement <$ char '-' 
    bprint :: Parser (BrainFk ())
    bprint = BPrint <$ char '.' 
    bget :: Parser (BrainFk ())
    bget = BGet <$ char ',' 
    bloop :: Parser (BrainFk ())
    bloop = BLoop <$ char '[' <*> (pack <$> many1 (char '<' <|> char '>' <|> char '+' <|> char '-' <|> char '.' <|> char ',')) <* char ']' 
    binvaildchar :: Parser (BrainFk ())
    binvaildchar = BInvalidChar <$ anyChar 
--(pack <$> many1 (char '<' <|> char '>' <|> char '+' <|> char '-' <|> char '.' <|> char ','))
showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r
