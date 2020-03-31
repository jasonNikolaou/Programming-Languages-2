-- | Programming Languages 2 (2020) 
import Control.Monad

-- | State monad, where the state is an integer number.

data State a = S (Integer -> (a, Integer))

instance Monad State where
  return x = S (\k -> (x, k))
  S z >>= f = S (\k -> let (x, k') = z k
                           S z' = f x
                      in   z' k')

get :: State Integer
get = S (\k -> (k, k))

set :: Integer -> State ()
set k' = S (\k -> ((), k'))

run :: Integer -> State a -> a
run k (S z) = fst $ z k

-- | Boilerplate code to make 'State' a legal monad.

instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

-- | A polymorphic n-ary tree data structure.

data Tree a = Node a [Tree a]
  deriving Show

-- | Function that annotates each node of a tree with the order in which
-- | a DFS traversal would visit it.

dfs :: Tree a -> Tree (a, Integer)
dfs = run 1 . aux
  where aux :: Tree a -> State (Tree (a, Integer))
        aux (Node x ts) = do
          k <- get
          set $ k+1
          ts' <- auxs ts
          return $ Node (x, k) ts'
        auxs :: [Tree a] -> State [Tree (a, Integer)]
        auxs [] = return []
        auxs (t : ts) = do
          t' <- aux t
          ts' <- auxs ts
          return $ t' : ts'

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
  print (dfs t1)
  print (dfs t2)

