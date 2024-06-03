{-# LANGUAGE LambdaCase #-}

module BF (runBF) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Parser
import Tape

runBF :: String -> [Int] -> Maybe [Int]
runBF source input = do
  bf <- parseBF' source
  pure $ runBF' bf input

type Runtime s = (STRef s [Int], STRef s [Int], TapeArray s, STRef s Int)

getRuntime :: [Int] -> ST s (Runtime s)
getRuntime input = do
  idx <- newSTRef 0
  input' <- newSTRef input
  arr <- newArray (0, 10) 0 :: ST s (TapeArray s)
  output <- newSTRef [] :: ST s (STRef s [Int])
  pure (input', output, arr, idx)

runBF' :: [BF] -> [Int] -> [Int]
runBF' bf input = runST $ do
  runtime@(_, output, _, _) <- getRuntime input

  act bf runtime

  modifySTRef output reverse
  readSTRef output
  where
    act :: [BF] -> Runtime s -> ST s ()
    act chunk runtime@(input', output, arr, idx) = forM_ chunk $ \case
      Inc -> incr idx arr
      Dec -> decr idx arr
      GoLeft -> goLeft idx arr
      GoRight -> goRight idx arr
      Write -> do
        val <- readSTRef input'
        case val of
          [] -> putVal (-1) idx arr
          (x : xs) -> putVal x idx arr *> writeSTRef input' xs
      Read -> do
        val <- getVal idx arr
        modifySTRef output (val :)
      Loop chunk' -> whileM (isTrue idx arr) (act chunk' runtime)

whileM :: (Monad m) => m Bool -> m a -> m ()
whileM p f = do
  isit <- p
  when isit $ f *> whileM p f
