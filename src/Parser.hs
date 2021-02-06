{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Parser(BrainFk(BRight,BLeft,Increment,Decrement,BPrint,BGet,BLoop,BNil,BError,BInvalidChar),parseBrainFk) where
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Data.Word

data BrainFk a  where
  BRight ::  BrainFk() -> BrainFk ()
  BLeft  ::  BrainFk() -> BrainFk ()
  Increment ::  BrainFk() -> BrainFk ()
  Decrement ::   BrainFk() ->BrainFk ()
  BPrint :: BrainFk() ->BrainFk ()
  BGet ::   BrainFk() -> BrainFk ()
  BLoop ::  Text -> BrainFk() -> BrainFk ()
  BInvalidChar :: BrainFk () -> BrainFk()
  BNil ::  BrainFk ()
  BError :: BrainFk()
instance Show (BrainFk r) where
  show (BRight r) = "BRight " ++ show r
  show (BLeft r) = "BLeft " ++ show r
  show (Increment r) = "Increment " ++ show r
  show (Decrement r) = "Decrement" ++ show r
  show (BPrint r) = "BPrint " ++ show r
  show (BGet r) = "BGet " ++ show r
  show (BLoop r r') = "BLoop " ++ show r ++ show r'
  show BNil= "BNil" 
  show (BInvalidChar r) = "BInvaildChar " ++ show r 
  show BError = "BError"
parseBrainFk ::   Text -> BrainFk ()
parseBrainFk s = let result = showParseResult $ parse (brainfkParser <* endOfInput) s `feed` pack ""
                  in case result of
                       Right x -> x
                       Left _ -> BError


brainfkParser :: Parser (BrainFk ())
brainfkParser = bright <|> bleft <|> increment <|> decrement <|> bprint <|> bget <|> bloop <|> binvaildchar <|> pure BNil
  where
    brainfk :: Parser (BrainFk ())
    brainfk = bright <|> bleft <|> increment <|> decrement <|> bprint <|> bget<|> bloop <|> binvaildchar <|>   pure BNil
    bright :: Parser (BrainFk ())
    bright = BRight <$ char '>' <*> brainfk
    bleft :: Parser (BrainFk ())
    bleft = BLeft <$ char '<' <*> brainfk
    increment :: Parser (BrainFk ())
    increment = Increment <$ char '+' <*> brainfk
    decrement :: Parser (BrainFk ())
    decrement = Decrement <$ char '-' <*> brainfk
    bprint :: Parser (BrainFk ())
    bprint = BPrint <$ char '.' <*> brainfk
    bget :: Parser (BrainFk ())
    bget = BGet <$ char ',' <*> brainfk
    bloop :: Parser (BrainFk ())
    bloop = BLoop <$ char '[' <*> (pack <$> many1 (char '<' <|> char '>' <|> char '+' <|> char '-' <|> char '.' <|> char ',')) <* char ']' <*> brainfk
    binvaildchar :: Parser (BrainFk ())
    binvaildchar = BInvalidChar <$ anyChar <*> brainfk
--(pack <$> many1 (char '<' <|> char '>' <|> char '+' <|> char '-' <|> char '.' <|> char ','))
showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r
