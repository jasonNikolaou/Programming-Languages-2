-- | Programming Languages 2 (2020)
-- | A polymorphic n-ary tree data structure.

data Tree a = Node a [Tree a]
  deriving Show

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

-- | Examples

t1 = Node 1 [ Node 2 [ Node 3 []
                     , Node 4 []
                     ]
            , Node 5 [ Node 6 [] ]
            ]

t2 = Node 'a' [ Node 'b' []
              , Node 'c' [ Node 'e' []
                         , Node 'f' []
                         ]
              , Node 'd' []
              ]

main = do
  print (bfs t1)
  print (bfs t2)
