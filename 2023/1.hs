parseNumber :: String -> (Int, String)
parseNumber number =
  case number of
    [] -> (0, [])
    (x : xs)
      | x `elem` ['0' .. '9'] -> (read [x], xs)
      | otherwise -> (0, xs)

parseNumberWithWords :: String -> (Int, String)
parseNumberWithWords number =
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
    _ -> parseNumber number

firstDigit :: (String -> (Int, String)) -> String -> Int
firstDigit parseNumber input =
  case input of
    [] -> 0
    (x : xs) ->
      let (number, rest) = parseNumber input
       in if number == 0
            then firstDigit parseNumber rest
            else number

lastDigit :: (String -> (Int, String)) -> String -> Int -> Int
lastDigit parseNumber input current =
  let (number, _) = parseNumber input
   in if number == 0
        then case input of
          [] -> current
          (x : xs) -> lastDigit parseNumber xs current
        else case input of
          [] -> number
          (x : xs) -> lastDigit parseNumber xs number

calibrationValue :: (String -> (Int, String)) -> [String] -> Int
calibrationValue parseNumber = foldr (\x -> (+) (firstDigit parseNumber x * 10 + lastDigit parseNumber x 0)) 0

main = do
  input <- getContents

  putStrLn ("Part 1: " <> show (calibrationValue parseNumber (lines input)))
  putStrLn ("Part 2: " <> show (calibrationValue parseNumberWithWords (lines input)))
