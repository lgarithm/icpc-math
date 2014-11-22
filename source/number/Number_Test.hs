module Main where
import Number
import Test.HUnit

test_list name = TestList . (map (TestLabel name))

t 1 = test_list "mmm" [ TestCase (assertEqual "mmm m" 2 (mmm 10 3 4)),
                           TestCase (assertEqual "mmm m" 0 (mmm 10 5 4)) ]

t 2 = test_list "nnn" [ TestCase (assertEqual "nnn n" 3 (nnn 10 (-37))),
                           TestCase (assertEqual "nnn n" 1 (nnn 10 1)),
                           TestCase (assertEqual "nnn n" 1 (nnn 10 11)) ]

t 3 = test_list "sqn" [ TestCase (assertEqual "sqn" 3 (sqn 10)),
                           TestCase (assertEqual "sqn" 3 (sqn 9)),
                           TestCase (assertEqual "sqn" 3 (sqn 15))]

t 4 = test_list "pfactors" [ TestCase (assertEqual "pfactors" [2, 3, 5] (pfactors 30)) ]

t 5 = test_list "fil" [ TestCase (assertEqual "fil" 5 (fil 60 18)) ]

t 6 = test_list "mfactor" [ TestCase (assertEqual "mfactor" 7 (mfactor 2 77)) ]

t 7 = test_list "" []

t 8 = test_list "" []

t 9 = test_list "" []

t 10 = test_list "crt" [ TestCase (assertEqual "crt" 23 (crt 3 5 2 7)),
                         TestCase (assertEqual "crt" 17 (crt 2 5 3 7)) ]

main = mapM_ runTestTT $ map t [1..10]
