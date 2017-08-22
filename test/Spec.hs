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

initial2 = unlines
  [ ".1....43."
  , "7........"
  , "...2549.."
  , "17..4.2.6"
  , "....9...3"
  , "..3..6.8."
  , "..147..6."
  , "...5.812."
  , ".9..6.3.4"
  ]

expected2 = unlines
  [ "519687432"
  , "724913658"
  , "386254917"
  , "178345296"
  , "652891743"
  , "943726581"
  , "231479865"
  , "467538129"
  , "895162374"
  ]

initial3 = unlines
  [ "..486..3."
  , "..1....9."
  , "8....9.6."
  , "5..2.6..1"
  , ".27..1..."
  , "....43..6"
  , ".5......."
  , "..9...4.."
  , "...4...15"
  ]

expected3 = unlines
  [ "974865132"
  , "261374598"
  , "835129764"
  , "543296871"
  , "627581349"
  , "198743256"
  , "452917683"
  , "319658427"
  , "786432915"
  ]

tests = testGroup "Unit tests"
  [ unitTest "1" expected1 initial1
  , unitTest "2" expected2 initial2
  , unitTest "3" expected3 initial3
  ]
