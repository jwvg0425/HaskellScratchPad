
data Tree a = Tree {value :: a, left :: Tree a, right :: Tree a} | Empty deriving Show

singleton :: (Ord a) => a -> Tree a
singleton value = Tree { value = value, left = Empty, right = Empty }

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty val = singleton val
insert tree val
    |value tree == val = tree
    |value tree > val = tree {left = insert (left tree) val}
    |value tree < val = tree {right = insert (right tree) val}

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldl insert Empty

preorder :: (Ord a) => Tree a -> [a]
preorder Empty = []
preorder tree = [value tree] ++ preorder (left tree) ++ preorder (right tree)

inorder :: (Ord a) => Tree a -> [a]
inorder Empty = []
inorder tree = inorder (left tree) ++ [value tree] ++ inorder (right tree)

postorder :: (Ord a) => Tree a -> [a]
postorder Empty = []
postorder tree = postorder (left tree) ++ postorder (right tree) ++ [value tree]

preToPost :: (Ord a) => [a] -> [a]
preToPost = postorder . treeFromList

preInToPost :: (Ord a) => [a] -> [a] -> [a]
preInToPost [] _ = []
preInToPost [a] _ = [a]
preInToPost preo ino = preInToPost preleft inleft ++ preInToPost preright inright ++ [head preo] 
    where inleft = takeWhile (/=head preo) ino
          preleft = take (length inleft) (tail preo)
          inright = drop (length inleft + 1) ino
          preright = drop (length inleft + 1) preo