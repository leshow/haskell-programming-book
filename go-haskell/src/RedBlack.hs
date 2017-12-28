module RedBlack (
    Tree,
    member,
    insert,
    singleton,
    delete
) where


data Color
    = R
    | B deriving Show

data Tree a
    = Nil
    | Node !Color !(Tree a) !a !(Tree a)
    deriving Show

{-
    1. No red node has a parent red node || red node has only black children
    2. every path from root to empty node contains same number of black nodes
    3. root and leaves are black
-}

member :: Ord a => a -> Tree a -> Bool
member _ Nil = False
member x (Node _ ta a tb)
    | x < a     = member x ta
    | x == a    = True
    | otherwise = member x tb

singleton :: a -> Tree a
singleton x = Node R Nil x Nil
{-# INLINE singleton #-}

insert :: Ord a => a -> Tree a -> Tree a
insert x = rootBlack . ins
    where
        ins Nil = singleton x
        ins (Node color ta a tb)
            | x < a     = balance color (ins ta) a tb
            | x == a    = Node color ta a tb
            | otherwise = balance color ta a (ins tb)

rootBlack :: Tree a -> Tree a
rootBlack (Node _ ta a tb) = Node B ta a tb

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (Node R (Node R ta x tb) y tb') z tb'' = Node R (Node B ta x tb) y (Node B tb' z tb'')
balance B (Node R ta x (Node R tb y tb')) z tb'' = Node R (Node B ta x tb) y (Node B tb' z tb'')
balance B ta x (Node R (Node R tb y tb') z tb'') = Node R (Node B ta x tb) y (Node B tb' z tb'')
balance B ta x (Node R tb y (Node R tb' z tb'')) = Node R (Node B ta x tb) y (Node B tb' z tb'')
balance color ta x tb = Node color ta x tb

delete :: Ord a => a -> Tree a -> Tree a
delete x = rootBlack . del x

del :: Ord a => a -> Tree a -> Tree a
del x t@(Node _ ta y tb)
    | x < y = delL x t
    | x > y = delR x t
    | otherwise = fuse ta tb

balanceLeft :: Tree a -> Tree a
balanceLeft (Node B (Node R ta x tb) y tb') = Node R (Node B ta x tb) y tb'
balanceLeft (Node B ta x (Node B tb y tb')) = balance B ta x (Node R tb y tb')
balanceLeft (Node B t1 y (Node R (Node B t2 u t3) z (Node B l value r))) =
        Node R (Node B t1 y t2) u (balance B t3 z (Node R l value r))

balanceRight :: Tree a -> Tree a
balanceRight (Node B t1 y (Node R t2 x t3)) = Node R t1 y (Node B t2 x t3)
balanceRight (Node B (Node B t1 z t2) y t3) = balance B (Node R t1 z t2) y t3
balanceRight (Node B (Node R (Node B l value r) z (Node B t2 u t3)) y t4) =
    Node R (balance B (Node R l value r) z t2) u (Node B t3 y t4)

delL :: (Ord a) => a -> Tree a -> Tree a
delL x (Node R t1 y t2) = Node R (del x t1) y t2
delL x (Node B t1 y t2) = balanceLeft $ Node B (del x t1) y t2

delR :: (Ord a) => a -> Tree a -> Tree a
delR x (Node B t1 y t2) = balanceRight $ Node B t1 y (del x t2)
delR x (Node R t1 y t2) = Node R t1 y (del x t2)

fuse :: Tree a -> Tree a -> Tree a
fuse Nil t = t
fuse t Nil = t
fuse t1@(Node B _ _ _) (Node R t3 y t4) = Node R (fuse t1 t3) y t4
fuse (Node R t1 x t2) t3@(Node B _ _ _) = Node R t1 x (fuse t2 t3)
fuse (Node R t1 x t2) (Node R t3 y t4)  =
    let s = fuse t2 t3
    in case s of
         (Node R s1 z s2) -> Node R (Node R t1 x s1) z (Node R s2 y t4)
         (Node B _ _ _)   -> Node R t1 x (Node R s y t4)
fuse (Node B t1 x t2) (Node B t3 y t4)  =
    let s = fuse t2 t3
    in case s of
            (Node R s1 z s2) -> Node R (Node B t1 x s1) z (Node B s2 y t4)
            (Node B _ _ _)   -> balanceLeft (Node B t1 x (Node B s y t4))
