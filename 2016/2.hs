import Data.Char

type Direction = Char

part1Pad :: Char -> Direction -> Char
part1Pad char direction =
  case direction of
    'L' ->
      if char == '1' || char == '4' || char == '7' then char else chr (ord char - 1)
    'U' ->
      if char == '1' || char == '2' || char == '3' then char else chr (ord char - 3)
    'D' ->
      if char == '7' || char == '8' || char == '9' then char else chr (ord char + 3)
    'R' ->
      if char == '3' || char == '6' || char == '9' then char else chr (ord char + 1)
    _ -> char

charsToJumpToLine :: Int -> Int -> Int
charsToJumpToLine a b =
  case (a, b) of
    (0, 1) -> 2
    (1, 0) -> -2
    (1, 2) -> 4
    (2, 1) -> -4
    (2, 3) -> 11
    (3, 2) -> -11
    (3, 4) -> 2
    (4, 3) -> -2
    _ -> error (show (a, b))

charsToJumpToNextLine :: Int -> Int
charsToJumpToNextLine a = charsToJumpToLine a (a + 1)

charsToJumpToPrevLine :: Int -> Int
charsToJumpToPrevLine a = charsToJumpToLine a (a - 1)

getLineNumber :: Char -> Int
getLineNumber c
  | c == '1' = 0
  | c == 'D' = 4
  | c >= '2' && c <= '4' = 1
  | c >= '5' && c <= '9' = 2
  | c >= 'A' && c <= 'C' = 3

part2Pad :: Char -> Direction -> Char
part2Pad char direction =
  case direction of
    'L' ->
      if char == '1' || char == '2' || char == '5' || char == 'A' || char == 'D'
        then char
        else chr (ord char - 1)
    'U' ->
      if char == '1' || char == '2' || char == '5' || char == '4' || char == '9'
        then char
        else chr (ord char + (charsToJumpToPrevLine . getLineNumber) char)
    'D' ->
      if char == '5' || char == 'A' || char == 'D' || char == '9' || char == 'C'
        then char
        else chr (ord char + (charsToJumpToNextLine . getLineNumber) char)
    'R' ->
      if char == '1' || char == '4' || char == '9' || char == 'C' || char == 'D'
        then char
        else chr (ord char + 1)
    _ -> char

moveOnPad :: (Char -> Direction -> Char) -> Char -> String -> String -> String
moveOnPad keyPad key keys input =
  case input of
    [] -> reverse keys
    '\n' : rest -> moveOnPad keyPad key (key : keys) rest
    c : rest -> moveOnPad keyPad (keyPad key c) keys rest

main = do
  input <- getContents

  putStrLn ("Part1: " ++ show (moveOnPad part1Pad '5' [] input))
  putStrLn ("Part2: " ++ show (moveOnPad part2Pad '5' [] input))
