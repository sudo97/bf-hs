module ParserTest (tests) where

import Parser
import Test.HUnit (Test (..), assertEqual, assertFailure)

readProducesRead :: Test
readProducesRead = TestCase $ do
  assertEqual "read" (Just [Read]) (parseBF' ".")

writeProducesWrite :: Test
writeProducesWrite = TestCase $ do
  assertEqual "write" (Just [Write]) (parseBF' ",")

goLeftProducesGoLeft :: Test
goLeftProducesGoLeft = TestCase $ do
  assertEqual "goLeft" (Just [GoLeft]) (parseBF' "<")

goRightProducesGoRight :: Test
goRightProducesGoRight = TestCase $ do
  assertEqual "goRight" (Just [GoRight]) (parseBF' ">")

incProducesInc :: Test
incProducesInc = TestCase $ do
  assertEqual "inc" (Just [Inc]) (parseBF' "+")

decProducesDec :: Test
decProducesDec = TestCase $ do
  assertEqual "dec" (Just [Dec]) (parseBF' "-")

loopProducesLoop :: Test
loopProducesLoop = TestCase $ do
  assertEqual "loop" (Just [Loop []]) (parseBF' "[]")

loopDoesntStopParser :: Test
loopDoesntStopParser = TestCase $ do
  assertEqual "loop doesn't stop parser, []+" (Just [Loop [], Inc]) (parseBF' "[]+")

loopParsesItsContents :: Test
loopParsesItsContents = TestCase $ do
  assertEqual "loop parses its contents" (Just [Loop [Inc]]) (parseBF' "[+]")

loopsCanNest :: Test
loopsCanNest = TestCase $ do
  assertEqual
    "loops can nest"
    ( Just
        [ Loop [Inc, Loop [Inc], Write]
        ]
    )
    (parseBF' "[+[+],]")

parseFailsForNonTerminatedLoop :: Test
parseFailsForNonTerminatedLoop = TestCase $ do
  assertEqual "parse fails for non-terminated loop" Nothing (parseBF' "[")

parseFailsForUnexpectedClosingBracket :: Test
parseFailsForUnexpectedClosingBracket = TestCase $ do
  assertEqual "parse fails for unexpected closing bracket" Nothing (parseBF' "]")

tests :: Test
tests =
  TestList
    [ readProducesRead,
      writeProducesWrite,
      goLeftProducesGoLeft,
      goRightProducesGoRight,
      incProducesInc,
      decProducesDec,
      loopProducesLoop,
      loopDoesntStopParser,
      loopParsesItsContents,
      loopsCanNest,
      parseFailsForNonTerminatedLoop,
      parseFailsForUnexpectedClosingBracket
    ]
