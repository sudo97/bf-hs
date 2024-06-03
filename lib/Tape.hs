{-# LANGUAGE RankNTypes #-}

module Tape where

import Control.Monad.ST
import Data.Array.ST (MArray (getBounds), STArray, modifyArray, readArray, writeArray)
import Data.STRef

type TapeArray s = STArray s Int Int

isTrue :: TapeAction Bool
isTrue idxRef arrRef = do
  idx <- readSTRef idxRef
  arr <- readArray arrRef idx
  pure $ arr /= 0

incr :: TapeAction ()
incr idxRef arrRef = do
  idx <- readSTRef idxRef
  modifyArray arrRef idx (+ 1)

decr :: TapeAction ()
decr idxRef arrRef = do
  idx <- readSTRef idxRef
  modifyArray arrRef idx (subtract 1)

goRight :: TapeAction ()
goRight idxRef arr = do
  (min', max') <- getBounds arr
  idx <- readSTRef idxRef
  if idx < max' then modifySTRef idxRef (+ 1) else writeSTRef idxRef min'

type TapeAction a = forall s. STRef s Int -> TapeArray s -> ST s a

goLeft :: TapeAction ()
goLeft idxRef arr = do
  (min', max') <- getBounds arr
  idx <- readSTRef idxRef
  if idx > min' then modifySTRef idxRef (subtract 1) else writeSTRef idxRef max'

getVal :: TapeAction Int
getVal idxRef arr = do
  idx <- readSTRef idxRef
  readArray arr idx

putVal :: Int -> TapeAction ()
putVal val idxRef arr = do
  idx <- readSTRef idxRef
  writeArray arr idx val
