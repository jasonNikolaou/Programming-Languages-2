import Control.Monad
import Control.Monad.Par
import Data.List
import System.Environment
import Control.Exception
import Control.DeepSeq

multMod :: Integer -> Integer -> Integer -> Integer
multMod a b p = foldl' (\acc x -> x * acc `mod` p) 1 [a..b] `mod` p

binom :: (Integer, Integer, Integer) -> Integer
binom (n, k, p) =
    let nom = multMod (n-k+1) n p
        den = multMod 1 k p
        den_inverse = expMod den (p-2) p
    in nom * den_inverse `mod` p

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
    evaluate $ deep $ runPar $ parMap binom input
