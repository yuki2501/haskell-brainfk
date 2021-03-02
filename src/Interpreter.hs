{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter(Memory(Memory),runM',bright,bleft,increment,decrement,bprint,bget,binvalidChar,berror) where
import qualified Data.Text as T
import Parser
import Control.Monad.Skeleton
import Data.Word
import Data.Function
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

type M = Skeleton BrainFk
data Memory = Memory {left :: [Word8],pointer :: Word8, right :: [Word8]}

right' :: Memory -> Memory
right' (Memory l p []) = Memory (p:l) 0 []
right' (Memory l p (r:rs)) = Memory (p:l) r rs
left' :: Memory -> Memory
left'  (Memory [] p r) = Memory [] 0 (p:r)
left' (Memory (l:ls) p r) = Memory ls l (p:r)
increment' :: Memory -> Memory
increment' m = m{pointer = pointer m + 1}
decrement' :: Memory -> Memory
decrement' m = m{pointer = pointer m - 1}

runM' :: Skeleton BrainFk () -> StateT Memory IO ()
runM' = deboneBy $ \case
  BRight :>>= k -> modify right' >>= (runM' .  k)
  BLeft :>>= k -> modify left' >>= (runM' . k)
  Increment :>>= k -> modify increment' >>= (runM' . k)
  Decrement :>>= k -> modify decrement' >>= (runM' . k)
  BPrint :>>= k ->  get >>= (\(Memory l p r) -> lift (putChar (toEnum . fromEnum $ p))) >>= (runM' . k)
  BGet :>>= k -> lift getChar >>= (\char -> modify $ \(Memory l p r) -> Memory l (toEnum . fromEnum $ char) r) >>= (runM' . k)
  BLoop r :>>= k -> loop >>= (runM' . k)
    where
      program = parseBrainFk r
      loop = do
        pointer <- pointer<$>get
        if pointer == 0 then return ()
                        else mapM_ (runM' . bone)  program >> loop
  BNil :>>= k -> modify id >>= (runM' . k)
  BInvalidChar :>>= k -> modify id >>= (runM' . k)
  BError :>>= k -> lift (putStrLn "error") >>= (runM' . k)
  Return a -> pure ()
-- Monads 
bright = bone  BRight
bleft = bone BLeft
increment = bone Increment
decrement = bone Decrement
bprint = bone BPrint
bget = bone BGet
bloop = bone  BLoop
binvalidChar = bone BInvalidChar
berror = bone BError
