module TapeTest (tests) where

import Control.Monad (forM_, replicateM)
import Control.Monad.ST
import Data.Array.ST (MArray (getBounds), STArray, newArray, readArray, writeArray)
import Data.Functor.Identity (Identity (runIdentity))
import Data.STRef
import DiffList
import GHC.Arr (STArray (STArray))
import Tape
import Test.HUnit (Test (..), assertEqual, assertFailure)

isTrueShowsTrueForNonZeroElt :: Test
isTrueShowsTrueForNonZeroElt = TestCase $ do
  assertEqual "isTrue shows true for non-zero element" True $ runST $ do
    runtime@(_, _, arr, _) <- getRuntime []
    writeArray arr 0 1
    isTrue runtime

isTrueShowsZeroForZeroElt :: Test
isTrueShowsZeroForZeroElt = TestCase $ do
  assertEqual "isTrue shows zero for zero element" False $ runST $ do
    runtime <- getRuntime []
    isTrue runtime

incrIncrementsValueAtCurrIndex :: Test
incrIncrementsValueAtCurrIndex = TestCase $ do
  assertEqual "incr increments value at current index" (1, 0) $ runST $ do
    runtime@(_, _, arr, idx) <- getRuntime []
    incr runtime
    value <- readArray arr 0
    idx' <- readSTRef idx
    pure (value, idx')

decrDecrementsValueAtCurrIndex :: Test
decrDecrementsValueAtCurrIndex = TestCase $ do
  assertEqual "decr decrements value at current index" (-1, 0) $ runST $ do
    runtime@(_, _, arr, idx) <- getRuntime []
    decr runtime
    value <- readArray arr 0
    idx' <- readSTRef idx
    pure (value, idx')

goRightMovesIndexRight :: Test
goRightMovesIndexRight = TestCase $ do
  assertEqual "goRight moves index right" 1 $ runST $ do
    runtime@(_, _, _, idx) <- getRuntime []
    goRight runtime
    readSTRef idx

goRightDoesNotOverflow :: Test
goRightDoesNotOverflow = TestCase $ do
  assertEqual "goRight does not overflow" 0 $ runST $ do
    runtime@(_, _, arr, idx) <- getRuntime []
    (minIdx, maxIdx) <- getBounds arr
    forM_ [minIdx .. maxIdx] . const $ goRight runtime
    readSTRef idx

goLeftMovesIndexLeft :: Test
goLeftMovesIndexLeft = TestCase $ do
  assertEqual "goLeft moves index left" 0 $ runST $ do
    runtime@(_, _, _, idx) <- getRuntime []
    goRight runtime
    goLeft runtime
    readSTRef idx

goLeftDoesNotUnderflow :: Test
goLeftDoesNotUnderflow = TestCase $ do
  assertEqual "goLeft does not underflow" 10 $ runST $ do
    runtime@(_, _, _, idx) <- getRuntime []
    goLeft runtime
    readSTRef idx

stdoutPutsCurrIndexValToOutput :: Test
stdoutPutsCurrIndexValToOutput = TestCase $ do
  assertEqual "stdout puts current index value to output" (Just 420) $ runST $ do
    runtime@(_, output, arr, idx) <- getRuntime []
    writeArray arr 3 420
    writeSTRef idx 3
    stdout runtime
    safeHead . toList <$> readSTRef output

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

stdinTakesOneItemAndPutsAtCurrIndex :: Test
stdinTakesOneItemAndPutsAtCurrIndex = TestCase $ do
  assertEqual "stdin takes one item and puts it at current index" (4, 20) $ runST $ do
    runtime@(inp, _, arr, idx) <- getRuntime [4, 20]
    stdin runtime
    value1 <- readArray arr 0
    writeSTRef idx 1
    value2 <- readArray arr 0
    pure (value1, value2)

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
      stdoutPutsCurrIndexValToOutput
      -- putValSetsValueAtCurrIndex
    ]
