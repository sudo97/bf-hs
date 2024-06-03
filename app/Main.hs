module Main where

-- import Tape

-- import Control.Monad
-- import Control.Monad.ST
-- import Data.Array.ST
-- import Data.STRef

-- readInput :: STRef s [Int] -> ST s Int
-- readInput input = do
--   inp' <- readSTRef input
--   case inp' of
--     [] -> pure (-1)
--     (x : xs) -> do
--       writeSTRef input xs
--       pure x

-- output :: STRef s [Int] -> Int -> ST s ()
-- output result x = modifySTRef result (x :)

-- computation :: ST s [Int]
-- computation = do
--   input <- newSTRef [1, 2, 3, 4, 5]
--   result <- newSTRef 0
--   pointer <- newSTRef 0
--   tape <- newArray_ (0, 100) :: ST s (STUArray s Int Int)
--   pure []

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

-- print $ runST computation
