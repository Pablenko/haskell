module Main where
import Sorting
import Test.HUnit
import qualified System.Exit as Exit
 
test_quicksort_one_elem_list :: Test
test_quicksort_one_elem_list = TestCase (assertEqual "quicksort 1 elem" [1] (quicksort [1]))

test_quicksort_list :: Test
test_quicksort_list = TestCase (assertEqual "quicksort list" [-12, -5, -2, 0, 1, 1, 10] (quicksort [-2, 1, -5, 10, -12, 1, 0]))
 
tests :: Test
tests = TestList [
    TestLabel "test_quicksort_one_elem_list" test_quicksort_one_elem_list,
    TestLabel "test_quicksort_list" test_quicksort_list
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
