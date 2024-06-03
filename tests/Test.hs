module Main (main) where

import qualified DiffListTest
import qualified ParserTest
import qualified System.Exit as Exit
import qualified TapeTest
import Test.HUnit

tests =
  TestList
    [ TestLabel "Tape" TapeTest.tests,
      TestLabel "Parser" ParserTest.tests,
      TestLabel "DiffList" DiffListTest.tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result > 0 || failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess