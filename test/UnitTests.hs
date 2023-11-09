module UnitTests where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual)
import Lib2

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [ testGroup "Unit Tests"
      [ testCase "Test won" test_won
      , testCase "Test empty" test_empty
      , testCase "Test prompt" test_prompt
      ]
  ]

test_won :: Assertion
test_won = assertBool "Test failed" (Lib2.won [['X', 'O', ' '], ['O', 'X', ' '], ['O', 'X', 'X']] 'X')

test_empty :: Assertion
test_empty = assertEqual "Should be an empty grid" Lib2.empty [[' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' ']]

test_prompt :: Assertion
test_prompt = assertEqual "Prompt for player X" (Lib2.prompt 'X') "Player X, enter your move: "
