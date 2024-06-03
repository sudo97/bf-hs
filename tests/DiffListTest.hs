{-# LANGUAGE TypeApplications #-}

module DiffListTest (tests) where

import DiffList (appendItem, fromList, toList)
import Test.HUnit (Test (..), assertEqual)

tests :: Test
tests =
  TestList
    [ TestCase $ assertEqual "toList is just a simple list" [] (toList @Int mempty),
      TestCase $ assertEqual "fromList builds a DiffList" [1, 2, 3] (toList . fromList $ [1, 2, 3]),
      TestCase $ assertEqual "append appends" [1, 2, 3] (toList $ fromList [1, 2] <> fromList [3]),
      TestCase $ assertEqual "append Item adds an Item" [1, 2, 3] (toList $ appendItem 3 (fromList [1, 2]))
    ]
