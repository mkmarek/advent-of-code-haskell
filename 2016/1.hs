import Data.Char
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()

data Move
  = LeftMove Int
  | RightMove Int
  deriving (Show)

parse :: String -> [Move]
parse input =
  map mapToMove (words input)

mapToMove :: String -> Move
mapToMove str =
  case str of
    'L' : rest ->
      let n = (read . filter isNumber) rest :: Int
       in LeftMove n
    'R' : rest ->
      let n = (read . filter isNumber) rest :: Int
       in RightMove n

part1 :: Int -> Int -> Int -> [Move] -> Int
part1 x y direction input =
  case input of
    [] -> abs x + abs y
    m : rest ->
      let (direction', steps) = case m of
            LeftMove n -> ((direction - 1 + 4) `mod` 4, n)
            RightMove n -> ((direction + 1) `mod` 4, n)
       in case direction' of
            0 -> part1 x (y - steps) direction' rest
            1 -> part1 (x + steps) y direction' rest
            2 -> part1 x (y + steps) direction' rest
            3 -> part1 (x - steps) y direction' rest

part2 :: Int -> Int -> Int -> Set (Int, Int) -> [Move] -> Int
part2 x y direction history input =
  case input of
    [] -> abs x + abs y
    m : rest ->
      case m of
        LeftMove n -> part2Movement x y ((direction - 1 + 4) `mod` 4) history rest n
        RightMove n -> part2Movement x y ((direction + 1) `mod` 4) history rest n

part2Movement :: Int -> Int -> Int -> Set (Int, Int) -> [Move] -> Int -> Int
part2Movement x y direction history input n
  | Set.member (x, y) history = abs x + abs y
  | n > 0 = case direction of
      0 -> part2Movement x (y - 1) direction (Set.insert (x, y) history) input (n - 1)
      1 -> part2Movement (x + 1) y direction (Set.insert (x, y) history) input (n - 1)
      2 -> part2Movement x (y + 1) direction (Set.insert (x, y) history) input (n - 1)
      3 -> part2Movement (x - 1) y direction (Set.insert (x, y) history) input (n - 1)
  | otherwise = part2 x y direction history input

main = do
  input <- getContents

  putStrLn ("Part1: " ++ show ((part1 0 0 0 . parse) input))
  putStrLn ("Part2: " ++ show ((part2 0 0 0 (Set.fromList []) . parse) input))
