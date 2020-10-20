import Data.Char
import System.IO
import Text.Read
import Data.Map.Strict (empty, (!), insert, keys)

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq
data Constraint = Constraint Type Type deriving Show
data Unifier = Unifier Type Type | TypeError deriving Show

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Type inference

typeInfer nextId gamma (Evar str) = (nextId, Tvar (gamma ! str), [])
typeInfer nextId gamma (Eabs str expr) =
  let (nextId', exprType, c) = typeInfer (nextId+1) (insert str nextId gamma) expr
  in (nextId', Tfun (Tvar nextId) exprType, c)
typeInfer nextId gamma (Eapp expr1 expr2) =
  let (nextId', expr1Type, c1) = typeInfer nextId gamma expr1
      (nextId'', expr2Type, c2) = typeInfer nextId' gamma expr2
  in (nextId'' + 1, Tvar nextId'', Constraint expr1Type (Tfun expr2Type (Tvar nextId'')) : (c1 ++ c2))

-- Unify help functions
isTvar :: Type -> Bool
isTvar t = case t of
  (Tvar _) -> True
  otherwise -> False

isTfun :: Type -> Bool
isTfun t = case t of
  (Tfun _ _) -> True
  otherwise -> False

contains :: Type -> Type -> Bool
contains (Tvar t1) (Tvar t2) = t1 == t2
contains t1 (Tfun t21 t22) = contains t1 t21 || contains t1 t22

replace :: Type -> Type -> Type -> Type
replace (Tvar a) t2 (Tvar t3) = if a == t3 then t2 else Tvar t3
replace t1 t2 (Tfun t3 t4) = Tfun (replace t1 t2 t3) (replace t1 t2 t4)

replaceAll :: Type -> Type -> [Constraint] -> [Constraint]
replaceAll _ _ [] = []
replaceAll t1 t2 (Constraint t3 t4:cs) = (Constraint (replace t1 t2 t3) (replace t1 t2 t4)) : replaceAll t1 t2 cs

-- Unify (W algorithm)

unify :: [Constraint] -> [Unifier]
unify [] = []
unify (Constraint t1 t2 : c) =
  if t1 == t2 then unify c
  else if isTvar t1 && not (contains t1 t2) then
    (unify $ replaceAll t1 t2 c) ++ [(Unifier t1 t2)]
  else if isTvar t2 && not (contains t2 t1) then
    (unify $ replaceAll t2 t1 c) ++ [(Unifier t2 t1)]
  else if isTfun t1 && isTfun t2 then
    let Tfun t11 t12 = t1
        Tfun t21 t22 = t2
    in unify $ (Constraint t11 t21) : (Constraint t12 t22) : c
  else [TypeError]

-- Apply unification
typeError :: [Unifier] -> Bool
typeError [] = False
typeError (TypeError : us) = True
typeError (_ : us) = typeError us

applyOneUnif :: Unifier -> Type -> Type
applyOneUnif (Unifier t1 t2) t = replace t1 t2 t

applyUnifs :: Type -> [Unifier] -> Type
applyUnifs t us = foldr (.) id (map applyOneUnif us) t

-- Help functions
select3 (_, _, x) = x
select23 (_, x, y) = (x, y)

replaceNums next m (Tvar t) =
  if t `elem` keys m then (next, m, Tvar (m ! t))
  else (next + 1, insert t next m, Tvar next)
replaceNums next m (Tfun t1 t2) =
  let (next', m', t1') = replaceNums next m t1
      (next'', m'', t2') = replaceNums next' m' t2
  in (next'', m'', Tfun t1' t2')

solve :: (Type, [Constraint]) -> String
solve (t, c) = let unif = unify c
               in if typeError unif then "type error"
                  else show $ select3 $ replaceNums 0 empty (applyUnifs t unif)
-- Main program

readOne = do
  s <- getLine
  let e = read s :: Expr
  return e

count n m = sequence $ take n $ repeat m

main = do
  n <- readLn
  exprs <- count n readOne
  let input = map (select23 . (typeInfer 0 empty)) exprs
  let ans = map solve input
  putStr $ unlines ans
