module Main where
import Bst
import Test.HUnit
import qualified System.Exit as Exit
 
test_bst_one_elem :: Test
test_bst_one_elem = TestCase (assertEqual "test_bst_one_elem" (Node 1 EmptyTree EmptyTree) (bstInsert 1 EmptyTree))

test_bst_contains_elem :: Test
test_bst_contains_elem = TestCase (assertEqual "test_bst_contains_elem" True (1 `bstElem` foldr bstInsert EmptyTree [1, 2, 3, 4]))

test_bst_doesnt_contain_elem :: Test
test_bst_doesnt_contain_elem = TestCase (assertEqual "test_bst_doesnt_contain_elem" False (5 `bstElem` foldr bstInsert EmptyTree [1, 2, 3, 4]))

test_bst_remove_elem_length_one_tree :: Test
test_bst_remove_elem_length_one_tree = TestCase (assertEqual "test_bst_remove_elem_length_one_tree" EmptyTree (bstDelete 1 $ foldr bstInsert EmptyTree [1]))

test_bst_remove_elem :: Test
test_bst_remove_elem = TestCase (assertEqual "test_bst_remove_elem_length_one_tree" (foldr bstInsert EmptyTree [-1, 2]) (bstDelete 1 $ foldr bstInsert EmptyTree [-1, 1, 2]))

tests :: Test
tests = TestList [
    TestLabel "test_bst_one_elem" test_bst_one_elem,
    TestLabel "test_bst_contains_elem" test_bst_contains_elem,
    TestLabel "test_bst_doesnt_contain_elem" test_bst_doesnt_contain_elem,
    TestLabel "test_bst_remove_elem_length_one_tree" test_bst_remove_elem_length_one_tree,
    TestLabel "test_bst_remove_elem" test_bst_remove_elem
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
