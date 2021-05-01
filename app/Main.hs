module Main where
import Parser
import Interpreter
import Data.Text
import Data.Word
import Control.Monad.Skeleton
import System.IO
import Control.Monad.Trans.State.Strict
import Debug.Trace
main :: IO ()
main = do
  loop
    where
      loop :: IO ()
      loop = do
        putStr ('\n':"haskell-BrainFk>")
        hFlush stdout
        bf <- parseBrainFk . pack <$> getLine
        evalStateT (mapM_ (runM'  . bone) bf) initMemory
        loop

initMemory :: Memory
initMemory = Memory [] 0 []


