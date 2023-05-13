module Bst(BstTree (..), bstInsert, bstElem, bstDelete) where

data BstTree a = EmptyTree | Node a (BstTree a) (BstTree a) deriving (Show, Eq)


oneElemBst :: a -> BstTree a
oneElemBst x = Node x EmptyTree EmptyTree 

bstInsert :: (Ord a) => a -> BstTree a -> BstTree a
bstInsert x EmptyTree = oneElemBst x
bstInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (bstInsert x left) right
    | x > a  = Node a left (bstInsert x right)


deleteNode :: (Ord a) => BstTree a -> BstTree a 
deleteNode (Node a EmptyTree right) = right
deleteNode (Node a left EmptyTree) = left
deleteNode (Node a left right) = (Node newNode left right)
    where newNode = leftistElement right

leftistElement :: (Ord a) => BstTree a -> a
leftistElement (Node a EmptyTree _) = a
leftistElement (Node _ left _) = leftistElement left

bstDelete :: (Ord a) => a -> BstTree a -> BstTree a
bstDelete x EmptyTree = EmptyTree
bstDelete x (Node a left right)
    | x == a = deleteNode (Node x left right)
    | x < a  = Node a (bstDelete x left) right
    | x > a  = Node a left (bstDelete x right)


bstElem :: (Ord a) => a -> BstTree a -> Bool
bstElem x EmptyTree = False
bstElem x (Node a left right)
    | x == a = True
    | x < a  = bstElem x left 
    | x > a  = bstElem x right


instance Functor BstTree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
