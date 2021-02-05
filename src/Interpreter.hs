{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
module Interpreter where
import qualified Data.Text as T
import Parser
import Control.Monad.Skeleton
import Data.Word
import Data.Function
type M = Skeleton BrainFk
data Memory = Memory {left :: [Word8],pointer :: Word8, right :: [Word8]}

bright :: Memory -> Memory
bright (Memory l p (r:rs)) = Memory (p:l) r rs
bleft :: Memory -> Memory
bleft (Memory (l:ls) p r) = Memory ls l (p:r)
increment :: Memory -> Memory
increment m = m{pointer = pointer m + 1}
decrement :: Memory -> Memory
decrement m = m{pointer = pointer m - 1}

runM ::  Skeleton BrainFk ()-> Memory -> IO ((),Memory)
runM m memory = case debone m of
                     BRight r :>>= k -> runM  (bone r >> k ()) (bright memory)  
                     BLeft r :>>= k -> runM  (bone r >> k()) (bleft memory)
                     Increment r :>>= k -> runM   (bone r >> k()) (increment memory)
                     Decrement r :>>= k -> runM  (bone r >> k()) (decrement memory)
                     BPrint r :>>= k -> do
                       putChar . toEnum . fromEnum $ pointer memory
                       putChar '\n'
                       runM (bone r >> k()) memory
                     BGet r :>>= k -> do
                       x <- fmap (toEnum . fromEnum) getChar
                       runM   (bone r >> k ()) (memory{pointer = x})
                     BLoop r r' :>>= k -> do
                       let pointer' = pointer memory
                       flip fix memory \loop n  ->
                         if pointer n == (0::Word8) then runM  (bone r' >> k ()) n
                                             else do
                                               result <- runM (bone (parseBrainFk r)) n
                                               let state = snd result
                                               loop state 
                     BNil :>>= k -> runM (k ()) memory
                     Return a -> pure ((),memory)


