{-# LANGUAGE RankNTypes #-}

module Tape where

import Control.Monad.ST
import Data.Array.ST (MArray (getBounds), STArray, modifyArray, newArray, readArray, writeArray)
import Data.STRef
import DiffList

type Runtime s = (STRef s [Int], STRef s (DiffList Int), TapeArray s, STRef s Int)

type TapeArray s = STArray s Int Int

type TapeAction a = forall s. Runtime s -> ST s a

isTrue :: TapeAction Bool
isTrue (_, _, arr, idx) = do
  idx' <- readSTRef idx
  (/= 0) <$> readArray arr idx'

incr :: TapeAction ()
incr (_, _, arr, idx) = do
  idx' <- readSTRef idx
  modifyArray arr idx' (+ 1)

decr :: TapeAction ()
decr (_, _, arr, idx) = do
  idx' <- readSTRef idx
  modifyArray arr idx' (subtract 1)

goRight :: TapeAction ()
goRight (_, _, arr, idxRef) = do
  (min', max') <- getBounds arr
  idx <- readSTRef idxRef
  if idx < max' then modifySTRef idxRef (+ 1) else writeSTRef idxRef min'

goLeft :: TapeAction ()
goLeft (_, _, arr, idxRef) = do
  (min', max') <- getBounds arr
  idx <- readSTRef idxRef
  if idx > min' then modifySTRef idxRef (subtract 1) else writeSTRef idxRef max'

stdin :: TapeAction ()
stdin (inp, _, arr, idxRef) = do
  inp' <- readSTRef inp
  case inp' of
    [] -> error "No input"
    x : xs -> do
      writeSTRef inp xs
      idx <- readSTRef idxRef
      writeArray arr idx x

stdout :: TapeAction ()
stdout (_, out, arr, idxRef) = do
  idx <- readSTRef idxRef
  val <- readArray arr idx
  modifySTRef out (appendItem val)

getRuntime :: [Int] -> ST s (Runtime s)
getRuntime input = do
  idx <- newSTRef 0
  input' <- newSTRef input
  arr <- newArray (0, 10) 0 :: ST s (TapeArray s)
  output <- newSTRef mempty :: ST s (STRef s (DiffList Int))
  pure (input', output, arr, idx)