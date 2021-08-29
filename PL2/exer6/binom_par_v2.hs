import Control.Monad
import Control.Monad.Par
import Data.List
import Control.DeepSeq
import System.Environment
import Control.Exception

multMod :: Integer -> Integer -> Integer -> Integer
multMod a b p = foldl' (\acc x -> x * acc `mod` p) 1 [a..b] `mod` p

binom :: (Integer, Integer, Integer) -> Par Integer
binom (n, k, p) = do
    nom' <- spawnP $ multMod (n-k+1) n p
    den_inverse' <- spawnP $ expMod (multMod 1 k p) (p-2) p
    nom <- get nom'
    den_inverse <- get den_inverse'
    return $ nom * den_inverse `mod` p

readInts :: String -> (Integer, Integer, Integer)
readInts str = (n, k, p)
    where [n, k, p] = map read (words str)

expMod :: Integer -> Integer -> Integer -> Integer
expMod a 1 p = a `mod` p
expMod a b p
    | even b = r `mod` p
    | otherwise = (a * r) `mod` p
    where r = expMod (a * a `mod` p) (b `div` 2) p

deep a = deepseq a a

main = do
    t <- fmap (read::String -> Int) getLine
    input <- replicateM t $ fmap readInts getLine
    evaluate (length input)
    evaluate $ deep $ runPar $ parMapM binom input
