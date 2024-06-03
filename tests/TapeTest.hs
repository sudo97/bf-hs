module TapeTest (tests) where

import Control.Monad (forM_, replicateM)
import Control.Monad.ST
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Functor.Identity (Identity (runIdentity))
import Data.STRef
import GHC.Arr (STArray (STArray))
import Tape
import Test.HUnit (Test (..), assertEqual, assertFailure)

isTrueShowsTrueForNonZeroElt :: Test
isTrueShowsTrueForNonZeroElt = TestCase $ do
  assertEqual "isTrue shows true for non-zero element" True $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    writeArray arr 0 1
    idx <- newSTRef 0
    isTrue idx arr

isTrueShowsZeroForZeroElt :: Test
isTrueShowsZeroForZeroElt = TestCase $ do
  assertEqual "isTrue shows zero for zero element" False $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 0
    isTrue idx arr

incrIncrementsValueAtCurrIndex :: Test
incrIncrementsValueAtCurrIndex = TestCase $ do
  assertEqual "incr increments value at current index" (1, 0) $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 0
    incr idx arr
    value <- readArray arr 0
    idx' <- readSTRef idx
    pure (value, idx')

decrDecrementsValueAtCurrIndex :: Test
decrDecrementsValueAtCurrIndex = TestCase $ do
  assertEqual "decr decrements value at current index" (0, 0) $ runST $ do
    arr <- newArray (0, 10) 1 :: ST s (STArray s Int Int)
    idx <- newSTRef 0
    decr idx arr
    value <- readArray arr 0
    idx' <- readSTRef idx
    pure (value, idx')

goRightMovesIndexRight :: Test
goRightMovesIndexRight = TestCase $ do
  assertEqual "goRight moves index right" 1 $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 0
    goRight idx arr
    readSTRef idx

goRightDoesNotOverflow :: Test
goRightDoesNotOverflow = TestCase $ do
  assertEqual "goRight does not overflow" 0 $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 10
    goRight idx arr
    readSTRef idx

goLeftMovesIndexLeft :: Test
goLeftMovesIndexLeft = TestCase $ do
  assertEqual "goLeft moves index left" 0 $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 1
    goLeft idx arr
    readSTRef idx

goLeftDoesNotUnderflow :: Test
goLeftDoesNotUnderflow = TestCase $ do
  assertEqual "goLeft does not underflow" 10 $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 0
    goLeft idx arr
    readSTRef idx

getValGetsValueAtCurrIndex :: Test
getValGetsValueAtCurrIndex = TestCase $ do
  assertEqual "getValue gets value at current index" 420 $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 5
    writeArray arr 5 420
    getVal idx arr

putValSetsValueAtCurrIndex :: Test
putValSetsValueAtCurrIndex = TestCase $ do
  assertEqual "putVal sets value at current index" 420 $ runST $ do
    arr <- newArray (0, 10) 0 :: ST s (STArray s Int Int)
    idx <- newSTRef 5
    putVal 420 idx arr
    getVal idx arr

tests :: Test
tests =
  TestList
    [ isTrueShowsTrueForNonZeroElt,
      isTrueShowsZeroForZeroElt,
      incrIncrementsValueAtCurrIndex,
      decrDecrementsValueAtCurrIndex,
      goRightMovesIndexRight,
      goRightDoesNotOverflow,
      goLeftMovesIndexLeft,
      goLeftDoesNotUnderflow,
      getValGetsValueAtCurrIndex,
      putValSetsValueAtCurrIndex
    ]
