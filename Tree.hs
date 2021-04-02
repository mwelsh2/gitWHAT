module Tree where
import Data.Maybe (isNothing, fromJust)
import qualified Data.Tree as T
import Data.List

-- A Tree is either Empty or a Branch of type a with two parent trees
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)
-- A Polytree is a list of roots
type Polytree a = [Tree a]

-- Initializes an Empty tree
initTree :: Tree a
initTree = Empty

-- Initializes an empty Polytree
initPolytree :: Polytree a
initPolytree = []

-- Creates a node with given content and no parents
createNode :: a -> Tree a
createNode c =
    Branch c Empty Empty

-- Creates a polytree with a single node
createPolytreeWithNode :: a -> Polytree a
createPolytreeWithNode c =
    [Branch c Empty Empty]



-- Returns the specified node from the given tree, if it exists,
-- using a given key function that takes input of type a and returns an Int
getNodeKey :: (Eq a) => Int -> (a -> Int) -> Tree a -> Maybe a
getNodeKey _ _ Empty = Nothing
getNodeKey n f (Branch a b c)
    | n == (f a) = Just a
    | isNothing result = getNodeKey n f c
    | otherwise = result
    where
        result = getNodeKey n f b

-- Returns the specified node from the given Polytree, if it exists,
-- using a given key function that takes input of type a and returns an Int
getNodeKeyPolytree :: (Eq a) => Int -> (a -> Int) -> Polytree a -> Maybe a
getNodeKeyPolytree _ _ [] = Nothing
getNodeKeyPolytree n f (x:xs)
    | isNothing result = getNodeKeyPolytree n f xs
    | otherwise = result
    where result = getNodeKey n f x

-- Returns the specified node from the given tree, if it exists
getNode :: (Eq a) => a -> Tree a -> Maybe a
getNode _ Empty = Nothing
getNode n (Branch a b c) 
    | n == a = Just a
    | isNothing result = getNode n c 
    | otherwise = result
    where
        result = getNode n b

-- Returns the specified node from the given Polytree, if it exists
getNodePolytree :: (Eq a) => a -> Polytree a -> Maybe a
getNodePolytree _ [] = Nothing
getNodePolytree n (x:xs)
    | isNothing result = getNodePolytree n xs
    | otherwise = result
    where result = getNode n x



-- Returns entire tree of the specified node from the given tree
getTreeKey :: (Eq a) => Int -> (a -> Int) -> Tree a -> Maybe (Tree a)
getTreeKey _ _ Empty = Nothing
getTreeKey n f (Branch a b c) 
    | n == (f a) = Just (Branch a b c)
    | isNothing result = getTreeKey n f c 
    | otherwise = result
    where
        result = getTreeKey n f b

-- Returns entire tree of the specified node from the given Polytree
getTreeKeyPolytree :: (Eq a) => Int -> (a -> Int) -> Polytree a -> Maybe (Tree a)
getTreeKeyPolytree _ _ [] = Nothing
getTreeKeyPolytree n f (x:xs)
    | isNothing result = getTreeKeyPolytree n f xs
    | otherwise = result
    where
        result = getTreeKey n f x

-- Returns entire tree of the specified node from the given tree
getTree :: (Eq a) => a -> Tree a -> Maybe (Tree a)
getTree _ Empty = Nothing
getTree n (Branch a b c) 
    | n == a = Just (Branch a b c)
    | isNothing result = getTree n c 
    | otherwise = result
    where
        result = getTree n b

-- Returns entire tree of the specified node from the given Polytree
getTreePolytree :: (Eq a) => a -> Polytree a -> Maybe (Tree a)
getTreePolytree _ [] = Nothing
getTreePolytree n (x:xs)
    | isNothing result = getTreePolytree n xs
    | otherwise = result
    where
        result = getTree n x



-- Gets parents of a tree node
getParents :: Tree a -> [a]
getParents Empty = []
getParents (Branch _ (Branch a _ _) Empty) = [a]
getParents (Branch _ Empty (Branch a _ _)) = [a]
getParents (Branch _ (Branch a _ _) (Branch b _ _)) = [a, b]
getParents _ = []

-- Gets parents of a given NodeID in a tree
getNodeParents :: Eq a => Int -> (a -> Int) -> Tree a -> [a]
getNodeParents i f t =
    maybe [] getParents (getTreeKey i f t)

-- Gets parents of a given NodeID in a Polytree
getNodeParentsPolytree :: Eq a => Int -> (a -> Int) -> Polytree a -> [a]
getNodeParentsPolytree i f t =
    maybe [] getParents (getTreeKeyPolytree i f t)



-- Adds a given child, to the specified parent in the given tree
addOneParentKey :: (Eq a) => a -> Int -> (a -> Int) -> Tree a -> Maybe (Tree a)
addOneParentKey c p f t = 
    if isNothing r1 then Nothing 
    else Just (Branch c (fromJust r1) Empty)
    where 
        r1 = getTreeKey p f t

-- Adds a given child, to the specified parent in the given tree, returning a new Polytree
-- with the child as a root instead of the parent
addOneParentKeyPolytree :: (Eq a) => a -> Int -> (a -> Int) -> Polytree a -> Maybe (Polytree a)
addOneParentKeyPolytree c p f t = 
    let r1Maybe = getTreeKeyPolytree p f t
        r1 = if isNothing r1Maybe then Empty else (fromJust r1Maybe)
        n = (Branch c r1 Empty)
    in Just (n : (delete r1 t))

-- Adds a given child, to the specified parent in the given tree
addOneParent :: (Eq a) => a -> a -> Tree a -> Maybe (Tree a)
addOneParent c p t = 
    if isNothing r1 then Nothing 
    else Just (Branch c (fromJust r1) Empty)
    where 
        r1 = getTree p t

-- Adds a given child, to the specified parent in the given tree, returning a new Polytree
-- with the child as a root instead of the parent
addOneParentPolytree :: (Eq a) => a -> a -> Polytree a -> Maybe (Polytree a)
addOneParentPolytree c p t = 
    let r1Maybe = getTreePolytree p t
        r1 = if isNothing r1Maybe then Empty else (fromJust r1Maybe)
        n = (Branch c r1 Empty)
    in Just (n : (delete r1 t))



-- Adds a given child, to the two specified parents in the given tree
addTwoParentsKey :: (Eq a) => a -> Int -> Int -> (a -> Int) -> Tree a -> Maybe (Tree a)
addTwoParentsKey _ _ _ _ Empty = Nothing
addTwoParentsKey c p1 p2 f t = 
    if isNothing r1 || isNothing r2 then Nothing
    else Just (Branch c (fromJust r1) (fromJust r2))
    where
        r1 = getTreeKey p1 f t
        r2 = getTreeKey p2 f t

-- Adds a given child, to the two specified parents in the given Polytree, returning a new Polytree
-- with the child as a root instead of the parents
addTwoParentsKeyPolytree :: (Eq a) => a -> Int -> Int -> (a -> Int) -> Polytree a -> Maybe (Polytree a)
addTwoParentsKeyPolytree c p1 p2 f t = 
    let r1Maybe = getTreeKeyPolytree p1 f t
        r2Maybe = getTreeKeyPolytree p2 f t
        r1 = if isNothing r1Maybe then Empty else (fromJust r1Maybe)
        r2 = if isNothing r2Maybe then Empty else (fromJust r2Maybe)
        n = (Branch c r1 r2)
    in Just (n : (delete r1 (delete r2 t)))

-- Adds a given child, to the two specified parents in the given tree
addTwoParents :: (Eq a) => a -> a -> a -> Tree a -> Maybe (Tree a)
addTwoParents _ _ _ Empty = Nothing
addTwoParents c p1 p2 t = 
    if isNothing r1 || isNothing r2 then Nothing
    else Just (Branch c (fromJust r1) (fromJust r2))
    where
        r1 = getTree p1 t
        r2 = getTree p2 t

-- Adds a given child, to the two specified parents in the given Polytree, returning a new Polytree
-- with the child as a root instead of the parents
addTwoParentsPolytree :: (Eq a) => a -> a -> a -> Polytree a -> Maybe (Polytree a)
addTwoParentsPolytree c p1 p2 t = 
    let r1Maybe = getTreePolytree p1 t
        r2Maybe = getTreePolytree p2 t
        r1 = if isNothing r1Maybe then Empty else (fromJust r1Maybe)
        r2 = if isNothing r2Maybe then Empty else (fromJust r2Maybe)
        n = (Branch c r1 r2)
    in Just (n : (delete r1 (delete r2 t)))

toDataTree :: (Show a) => Tree a -> T.Tree [Char]
toDataTree Empty = T.Node "" []
toDataTree (Branch a t1 t2) = T.Node (show a) [toDataTree t1, toDataTree t2]

printTree :: (Show a) => Tree a -> String 
printTree t = T.drawTree (toDataTree t)

printPolytree :: (Show a) => Polytree a -> String
printPolytree pt = foldr (\t acc -> (printTree t)++"\n\n"++acc) "" pt
