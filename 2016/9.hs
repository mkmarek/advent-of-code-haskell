import Text.Read (Lexeme (String))

type Marker = (Int, Int)

parseMarker :: String -> (Marker, String)
parseMarker s =
  let (marker, rest) = break (== ')') s
      (a, b) = break (== 'x') marker
   in ((read a, read (tail b)), tail rest)

expandMarker :: Marker -> Int -> Int
expandMarker (chars, multiply) len = len * multiply

process :: String -> Int
process s =
  case s of
    '(' : rest ->
      let (marker, rest') = parseMarker rest
       in expandMarker marker (fst marker) + process (drop (fst marker) rest')
    c : rest -> 1 + process rest
    [] -> 0

process2 :: String -> Int
process2 s =
  case s of
    '(' : rest ->
      let (marker, rest') = parseMarker rest
       in expandMarker marker (process2 (take (fst marker) rest')) + process2 (drop (fst marker) rest')
    c : rest -> 1 + process2 rest
    [] -> 0

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\n' || c == '\t'

isNotWhitespace :: Char -> Bool
isNotWhitespace c = not (isWhitespace c)

main = do
  input <- getContents

  putStr "\nPart 1: \n"
  putStr (show (process (filter isNotWhitespace input)))

  putStr "\nPart 2: \n"
  putStr (show (process2 (filter isNotWhitespace input)))
  putStr "\n"
