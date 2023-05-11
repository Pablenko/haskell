module Main where
import Sorting
import Test.HUnit
import qualified System.Exit as Exit
 
test_quicksort_one_elem_list :: Test
test_quicksort_one_elem_list = TestCase (assertEqual "quicksort 1 elem" [1] (quicksort [1]))

test_quicksort :: Test
test_quicksort = TestCase (assertEqual "quicksort list" [-12, -5, -2, 0, 1, 1, 10] (quicksort [-2, 1, -5, 10, -12, 1, 0]))

test_mergesort :: Test
test_mergesort = TestCase (assertEqual "mergesort list" [-12, -10, 1, 10, 12] (mergesort [-10, 10, -12, 12, 1]))

test_bubblesort :: Test
test_bubblesort = TestCase (assertEqual "bubblesort list" [-6, -5, -4, -3, 7] (bubblesort [-5, -4, 7, -6, -3]))
 
tests :: Test
tests = TestList [
    TestLabel "test_quicksort_one_elem_list" test_quicksort_one_elem_list,
    TestLabel "test_quicksort" test_quicksort,
    TestLabel "test_mergesort" test_mergesort,
    TestLabel "test_bubblesort" test_bubblesort
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
