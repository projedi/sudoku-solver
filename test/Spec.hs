import Test.Tasty
import Test.Tasty.HUnit

import Lib (solve)

main = defaultMain tests

unitTest name expected initial = testCase name (assertEqual "" expected (solve initial))

initial1 = unlines
  [ "64..3...7"
  , "5.1.7.9.."
  , ".......1."
  , "..49.8.6."
  , ".8...3.2."
  , "...4....."
  , "4..157.3."
  , "2.83...4."
  , "75.....96"
  ]

expected1 = unlines
  [ "649831257"
  , "531672984"
  , "827549613"
  , "374928561"
  , "185763429"
  , "962415378"
  , "496157832"
  , "218396745"
  , "753284196"
  ]

tests = testGroup "Unit tests"
  [ unitTest "1" expected1 initial1
  ]
