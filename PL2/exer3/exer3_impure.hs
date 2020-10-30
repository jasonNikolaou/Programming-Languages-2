import Data.Array ((!), listArray, elems)
import Data.List (foldl', scanl')
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Int
import Data.Monoid
import System.IO

sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

dp n m = elems list
  where list = listArray (0, n) [f x | x <- [0..n]]
        f 0 = 2
        f 1 = 2
        f x = let filteredSums = takeWhile (>=0) $ map (\s -> x-s) sums
              in  foldl' (\acc s -> (acc + list ! s)) 0 filteredSums `mod` m

constructSums list m = tail $ scanl' (\acc x -> (acc+x) `mod` m) 0 list

solve sumsArr m (0, b) = ((sumsArr ! b) - 1) `mod` m
solve sumsArr m (a, b) = ((sumsArr ! b) - (sumsArr ! (a-1))) `mod` m

findMax = foldl' (\acc s -> let (_, b) = s in (max acc b)) 0

readInts :: IO [Int64]
readInts = map parse . C.words <$> C.getLine
 where parse s = let Just (n, _) = C.readInteger s
                 in fromIntegral n

readQueries :: IO [(Int,Int)]
readQueries = queries
 where parse s = let Just (n, _) = C.readInt s in n
       allInts = map parse . C.words <$> (C.hGetContents System.IO.stdin)
       split [] = []
       split (a:b:xs) = (a,b) : split xs
       queries = split <$> allInts

main = do
  (n:m:[]) <- readInts
  queries <- readQueries
  let max' = findMax queries
  let dpList = dp max' m
  let partialSums = constructSums dpList m
  let partialSumsArray = listArray (0, max') partialSums
  let answers = map (solve partialSumsArray m) queries
  answers `seq` hPutBuilder stdout $ unlines_ $ map int64Dec answers
  where
    unlines_ = mconcat . map (<> charUtf8 '\n')
