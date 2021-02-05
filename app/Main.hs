module Main where
import Parser
import Interpreter
import Data.Text
import Data.List
import Data.Word
import Control.Monad.Skeleton
import System.IO
main :: IO ()
main = do
  loop

initMemory :: Memory
initMemory = Memory (repeat (0::Word8)) 0 (repeat (0::Word8))

loop :: IO ()
loop = do
  putStr "haskell-BrainFk>"
  hFlush stdout
  bf <- getLine
  let parsedbf = parseBrainFk $ pack bf
  runM (bone parsedbf) initMemory
  loop


