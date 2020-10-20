import Data.List (foldl', scanl')
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Int
import Data.Monoid
import System.IO

sums = [1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535,131071,262143,524287]

helpFold p = foldl' (\acc list -> let hd = head list in (acc + hd) `mod` p) 0

check x = x == 3 || x == 7 || x == 15 || x == 31 || x == 63 || x == 127 || x == 255 || x == 511 || x == 1023 ||
          x == 2047 || x == 4095 || x == 8191 || x == 16383 || x == 32767 || x == 65535 || x == 131071 || x== 262143 || x == 524287

getDpList p = dp
      where dp = 2 : 2 : sumHeads 2 p [(tail dp)]
              where sumHeads x p lists = sumOfHeads : (nextIndex `seq` newPrevious `seq` sumHeads nextIndex p newPrevious)
                      where sumOfHeads = helpFold p lists
                            nextIndex = x + 1
                            tails = map tail lists
                            newPrevious = sumOfHeads `seq` nextIndex `seq` tails `seq` dp `seq`
                                          if (check nextIndex) then (dp : tails) else tails

readInts :: IO [Int64]
readInts = map parse . C.words <$> C.getLine
  where parse s = let Just (n, _) = C.readInteger s
                  in fromIntegral n

toTuple = (\(x:y:[]) -> (x,y))

findMax = foldl' (\acc s -> let [_, b] = s in (max acc b)) 0

takeElements max' p = take (fromIntegral max') (getDpList p)

constructSums list p =
  let partialSums = tail $ zip ([-1..]) (scanl' (\acc x -> (acc+x) `mod` p) 0 list)
  in partialSums `seq` Map.fromList partialSums -- Map.fromAscList is slower for an unknown reason...

solve m p [a, b] =
  let Just dp_a = if a == 0 then Just 1 else Map.lookup (a-1) m
      Just dp_b = Map.lookup b m
      ans = (dp_b - dp_a) `mod` p
  in ans

fastPrint answers = hPutBuilder stdout $ build  answers
build = foldr add_line mempty
   where add_line n b = int64Dec n <> charUtf8 '\n' <> b

main = do
  (n:m:[]) <- readInts
  queries <- replicateM (fromIntegral n) readInts
  -- let queries = map toTuple queries_
  let max' = (findMax queries) + 1
  let list = takeElements max' m
  let partialSums = constructSums list m
  let answers = map (solve partialSums m) queries

  queries `seq` list `seq` partialSums `seq` answers `seq` fastPrint answers
