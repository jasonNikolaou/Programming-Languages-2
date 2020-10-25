import Data.Array ((!), listArray, elems)
import Data.List (foldl', scanl')
import Control.Monad (replicateM)
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
        f x = let filteredSums = [s |s <- sums, x-s >= 0]
              in  foldl' (\acc s -> (acc + list ! (x-s))) 0 filteredSums `mod` m


constructSums list m = tail $ scanl' (\acc x -> (acc+x) `mod` m) 0 list

solve sumsArr m [a,b] = let dp_a = if a == 0 then 1 else sumsArr ! (a-1)
                            dp_b = sumsArr ! b
                        in (dp_b - dp_a) `mod` m

findMax = foldl' (\acc s -> let [_, b] = s in (max acc b)) 0

readInts :: IO [Int64]
readInts = map parse . C.words <$> C.getLine
 where parse s = let Just (n, _) = C.readInteger s
                 in fromIntegral n

fastPrint answers = hPutBuilder stdout $ build  answers
build = foldr add_line mempty
  where add_line n b = int64Dec n <> charUtf8 '\n' <> b

main = do
  (n:m:[]) <- readInts
  queries <- replicateM (fromIntegral n) readInts
  let max' = findMax(queries) + 1
  let dpList = dp max' m
  let partialSums = constructSums dpList m
  let partialSumsArray = listArray (0, max') partialSums
  let answers = map (solve partialSumsArray m) queries

  fastPrint answers
