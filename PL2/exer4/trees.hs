import Test.QuickCheck
import Control.Monad

data Tree a = Node a [Tree a]
    deriving (Show, Eq)

size :: (Tree a) -> Int
size (Node _ []) = 1
size (Node _ t)  = 1 + (sum . map size) t

height :: (Tree a) -> Int
height (Node _ []) = 1
height (Node _ t)  = 1 + (maximum . map height) t

arbWithSize 0 = do 
                x <- arbitrary
                return (Node x [])
arbWithSize n = frequency [ (1, do 
                                x <- arbitrary
                                return (Node x []))
                          , (9, do 
                                x <- arbitrary
                                (Positive m) <- arbitrary -- number of children
                                let k = n `div` m
                                ts <- replicateM m (arbWithSize k)
                                return (Node x ts))
                            ]

shrinkTree (Node _ []) = [] -- do nothing
shrinkTree (Node x ts) = 
    [(Node x [])] ++  -- reduce to a leaf
    ts ++             -- reduce to a child
    [(Node x ts') | ts' <- mapM shrinkTree ts] -- shrink children

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = sized arbWithSize
    shrink = shrinkTree

-- properties for the bfs and dfs functions

prop_tree_height f t = 
    (height (f t)) == (height t)
    where types = (t :: Tree Int)

prop_tree_size f t =
    (size (f t)) == (size t)
    where types = (t :: Tree Int)

prop_tree_root f t =
    let 
        Node r _ = f t
    in
        snd r == 1  
    where types = (t :: Tree Int)

-- properties for the merge function.

prop_tree_merge f g t1@(Node _ txs) t2@(Node _ tys) = 
    let 
        Node _ ts = f g t1 t2
        c1 = length ts  -- number of children of root in merged tree.
        c2 = length txs -- number of children of root in t1.
        c3 = length tys -- number of children of root in t2.
    in
        c1 == (max c2 c3)
    where types = (t1 :: Tree Int)
    
-- functions to test

-- | Function that annotates each node of a tree with the order in which
-- | a DFS traversal would visit it.

dfs :: Tree a -> Tree (a, Integer)
dfs t = fst (aux 1 t)
  where aux :: Integer -> Tree a ->
               (Tree (a, Integer), Integer)
        aux k (Node x ts) = (Node (x, k) ts', k')
          where (ts', k') = auxs (k+1) ts
        auxs :: Integer -> [Tree a] ->
                ([Tree (a, Integer)], Integer)
        auxs k [] = ([], k)
        auxs k (t : ts) = (t' : ts', k'')
          where (t', k') = aux k t
                (ts', k'') = auxs k' ts
                
-- | Function that annotates each node of a tree with the order in which
-- | a BFS traversal would visit it.

bfs :: Tree a -> Tree (a, Integer)
bfs t = t'
  where (t', ks') = aux ks t
        ks = 1 : ks'
        aux (k : ks) (Node x ts) = (Node (x, k) ts', (k+1) : ks')
          where (ts', ks') = auxs ks ts
        auxs ks [] = ([], ks)
        auxs ks (t : ts) = (t' : ts', ks'')
          where (t', ks') = aux ks t
                (ts', ks'') = auxs ks' ts

-- | Function that annotates each node of a tree with the number 42
-- | which is the answer to life, the universe and everything...

dummy :: Tree a -> Tree (a, Integer)
dummy (Node x []) = Node (x, 42) []
dummy (Node x ts) = Node (x, 42) ts' where
    ts' = map dummy ts

-- | Function that merges two trees using function f on corresponding nodes.

merge :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
merge f (Node x tsx) (Node y tsy) = 
    Node (f x y) $ zipTrees tsx tsy 
    where   zipTrees tsx [] = tsx
            zipTrees [] tsy = tsy
            zipTrees (tx: tsx) (ty: tsy) = (merge f tx ty) : zipTrees tsx tsy


wrong :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy

-- main program

main = do
    print "-- testing bfs function --"
    quickCheck (prop_tree_height bfs)
    quickCheck (prop_tree_size bfs)
    quickCheck (prop_tree_root bfs)
    print "-- testing dfs function --"
    quickCheck (prop_tree_height dfs)
    quickCheck (prop_tree_size dfs)
    quickCheck (prop_tree_root dfs)
    print "-- testing dummy function --"
    quickCheck (prop_tree_height dummy)
    quickCheck (prop_tree_size dummy)
    quickCheck (prop_tree_root dummy)
    print "-- testing merge function --"
    quickCheck (prop_tree_merge merge (+))
    print "-- testing wrong function --"
    quickCheck (prop_tree_merge wrong (+))