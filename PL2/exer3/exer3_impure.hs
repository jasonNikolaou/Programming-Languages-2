import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Int
import System.IO

sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

dp n m = elems list
  where list = listArray (0, n) [f x | x <- [0..n]]
        f 0 = 2
        f 1 = 2
        f x = let filteredSums = [s |s <- sums, x-s >= 0]
              in  foldl' (\acc s -> (acc + list ! (x-s))) 0 filteredSums `mod` m


constructSums list m = tail $ scanl' (\acc x -> (acc+x) `mod` m) 0 list

solve sumsArr m (a,b) = let dp_a = if a == 0 then 1 else sumsArr ! (a-1)
                            dp_b = sumsArr ! b
                        in (dp_b - dp_a) `mod` m

toTuple = (\(x:y:[]) -> (x,y))

readInts :: IO [Int64]
readInts = map parse . C.words <$> C.getLine
 where parse s = let Just (n, _) = C.readInteger s
                 in fromIntegral n

findMax = foldl' (\acc s -> let (_, b) = s in (max acc b)) 0

main = do
  (n:m:[]) <- readInts
  queries_ <- replicateM (fromIntegral n) readInts
  let queries = map toTuple queries_
  let max' = (findMax queries) + 2
  let dpList = dp max' m
  let partialSums = constructSums dpList m
  let partialSumsArray = listArray (0, max') partialSums
  let answers = map (solve partialSumsArray m) queries

  mapM_ print answers 
