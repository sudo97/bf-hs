{-# LANGUAGE LambdaCase #-}

module BF (runBF) where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.STRef
import DiffList
import Parser
import Tape

runBF :: String -> [Int] -> Maybe [Int]
runBF source input = do
  bf <- parseBF' source
  pure $ runBF' bf input

runBF' :: [BF] -> [Int] -> [Int]
runBF' bf input = runST $ do
  runtime@(_, output, _, _) <- getRuntime input
  act bf runtime
  toList <$> readSTRef output

whileM :: (Monad m) => m Bool -> m a -> m ()
whileM p f = do
  isit <- p
  when isit $ f *> whileM p f

act :: [BF] -> Runtime s -> ST s ()
act chunk runtime = forM_ chunk $ \case
  Inc -> incr runtime
  Dec -> decr runtime
  GoLeft -> goLeft runtime
  GoRight -> goRight runtime
  Write -> stdin runtime
  Read -> stdout runtime
  Loop chunk' -> whileM (isTrue runtime) (act chunk' runtime)
