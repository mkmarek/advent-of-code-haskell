import Data.Map qualified

mostCommonCharacterInColumn :: [String] -> Int -> Char
mostCommonCharacterInColumn s n =
  let map = foldr (\str acc -> Data.Map.insertWith (+) (str !! n) 1 acc) Data.Map.empty s
   in fst (Data.Map.foldrWithKey (\k el (k', el') -> if el > el' then (k, el) else (k', el')) ('_', 0) map)

part1 :: [String] -> Int -> String
part1 s n =
  if n < length (head s)
    then mostCommonCharacterInColumn s n : part1 s (n + 1)
    else ""

leastCommonCharacterInColumn :: [String] -> Int -> Char
leastCommonCharacterInColumn s n =
  let map = foldr (\str acc -> Data.Map.insertWith (+) (str !! n) 1 acc) Data.Map.empty s
   in fst (Data.Map.foldrWithKey (\k el (k', el') -> if el < el' then (k, el) else (k', el')) ('_', 1000) map)

part2 :: [String] -> Int -> String
part2 s n =
  if n < length (head s)
    then leastCommonCharacterInColumn s n : part2 s (n + 1)
    else ""

main :: IO ()
main = do
  input <- getContents

  print ("Part1: " ++ part1 (words input) 0)
  print ("Part1: " ++ part2 (words input) 0)
