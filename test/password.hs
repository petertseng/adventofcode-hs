module Main where

import AdventOfCode.Password (hasSafePair, nextPair)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (Counts(..), Test(..), assertBool, assertEqual, runTestTT)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

hasSafePairTests :: [Test]
hasSafePairTests = [
    TestCase (assertBool "pair sufficiently far is safe" (hasSafePair "abcc"))
  , TestCase (assertBool "pair insufficiently far is unsafe" (not (hasSafePair "abbc")))
  , TestCase (assertBool "pair at start is unsafe" (not (hasSafePair "bbcd")))
  , TestCase (assertBool "no pairs at all, obviously unsafe" (not (hasSafePair "abcd")))
  ]

nextPairTable :: [(String, String, String)]
nextPairTable = [
    ("disregard pair already there", "aaa", "abb")
  , ("incrementing z gives pair", "cbz", "cca")
  , ("incrementing more than one z", "dbzz", "dcaa")
  , ("change only last char", "xba", "xbb")
  , ("change last and second-to-last chars", "xbd", "xcc")
  , ("change second-to-last, pairing with third", "cbc", "cca")
  , ("skip confusing if pair before", "jhj", "jja")
  , ("skip confusing if pair after", "xhh", "xjj")
  ]

nextPair' :: String -> String
nextPair' = reverse . nextPair . reverse

nextPairTests :: [Test]
nextPairTests = map toTest nextPairTable
  where toTest (p, prev, next) = TestCase (assertEqual p next (nextPair' prev))

main :: IO ()
main = exitProperly $ runTestTT tests
  where tests = TestList [
            TestLabel "hasSafePair" (TestList hasSafePairTests)
          , TestLabel "nextPair" (TestList nextPairTests)
          ]
