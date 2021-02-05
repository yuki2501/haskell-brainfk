module Main where
import Parser
import Interpreter
import Data.Text
import Data.List
import Data.Word
import Control.Monad.Skeleton
main :: IO ()
main = do
  bf <- getLine
  let parsedbf = parseBrainFk $ pack bf
  runM  (bone parsedbf) initMemory
  pure ()

initMemory :: Memory
initMemory = Memory (repeat (0::Word8)) 0 (repeat (0::Word8))

