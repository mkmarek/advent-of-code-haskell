data IP = IP {characters :: String, hyper :: String}
  deriving (Show)

parseSingleIP :: String -> IP
parseSingleIP s =
  let (chars, hyp) = collectIPParts s False ("", "")
   in IP chars hyp

collectIPParts :: String -> Bool -> (String, String) -> (String, String)
collectIPParts s inH (c, h) =
  case s of
    [] -> (c, h)
    ch : rest ->
      if ch == '['
        then collectIPParts rest True (' ' : c, h)
        else
          if ch == ']'
            then collectIPParts rest False (c, ' ' : h)
            else
              if inH
                then collectIPParts rest inH (c, ch : h)
                else collectIPParts rest inH (ch : c, h)

hasABBA :: String -> Bool
hasABBA s =
  case s of
    a : b : c : d : rest -> (a == d && b == c && a /= b) || hasABBA (b : c : d : rest)
    _ -> False

hasABAandBAB :: String -> String -> Bool
hasABAandBAB s ss =
  case s of
    a : b : c : rest ->
      ((a == c && b /= c) && hasABA ss b a)
        || hasABAandBAB (b : c : rest) ss
    _ -> False

hasABA :: String -> Char -> Char -> Bool
hasABA s aa bb =
  case s of
    a : b : c : rest -> (a == aa && b == bb && c == aa) || hasABA (b : c : rest) aa bb
    _ -> False

isIPV7 :: IP -> Bool
isIPV7 ip = hasABBA (characters ip) && not (hasABBA (hyper ip))

hasSSL :: IP -> Bool
hasSSL ip = hasABAandBAB (characters ip) (hyper ip)

parse :: String -> [IP]
parse s =
  map parseSingleIP (words s)

main :: IO ()
main = do
  input <- getContents

  print ("Part1: " ++ show (length (filter isIPV7 (parse input))))
  print ("Part2: " ++ show (length (filter hasSSL (parse input))))
