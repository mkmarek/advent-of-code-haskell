parseNumber :: String -> (Int, String)
parseNumber number =
  case number of
    [] -> (0, [])
    ('o' : 'n' : 'e' : rest) -> (1, rest)
    ('t' : 'w' : 'o' : rest) -> (2, rest)
    ('t' : 'h' : 'r' : 'e' : 'e' : rest) -> (3, rest)
    ('f' : 'o' : 'u' : 'r' : rest) -> (4, rest)
    ('f' : 'i' : 'v' : 'e' : rest) -> (5, rest)
    ('s' : 'i' : 'x' : rest) -> (6, rest)
    ('s' : 'e' : 'v' : 'e' : 'n' : rest) -> (7, rest)
    ('e' : 'i' : 'g' : 'h' : 't' : rest) -> (8, rest)
    ('n' : 'i' : 'n' : 'e' : rest) -> (9, rest)
    (x : xs)
      | x `elem` ['0' .. '9'] -> (read [x], xs)
      | otherwise -> (0, xs)

firstDigit :: String -> Int
firstDigit [] = 0
firstDigit input =
  let (number, rest) = parseNumber input
   in if number == 0
        then firstDigit rest
        else number

lastDigit :: String -> Int -> Int
lastDigit input current =
  let (number, _) = parseNumber input
   in if number == 0
        then case input of
          [] -> current
          (x : xs) -> lastDigit xs current
        else case input of
          [] -> number
          (x : xs) -> lastDigit xs number

calibrationValue :: [String] -> Int
calibrationValue = foldr (\x -> (+) (firstDigit x * 10 + lastDigit x 0)) 0

main = do
  input <- getContents

  putStrLn ("Part1: " ++ show (calibrationValue (lines input)))
  putStrLn "Part2: "
