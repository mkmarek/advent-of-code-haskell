part1 :: [Int] -> Int
part1 sides =
  case sides of
    [] -> 0
    a : b : c : rest ->
      if a + b > c && b + c > a && c + a > b
        then part1 rest + 1
        else part1 rest

reorganizeToColumns :: [Int] -> [Int]
reorganizeToColumns sides =
  case sides of
    [] -> []
    a1 : a2 : a3 : b1 : b2 : b3 : c1 : c2 : c3 : rest ->
      a1 : b1 : c1 : a2 : b2 : c2 : a3 : b3 : c3 : reorganizeToColumns rest

parse :: String -> [Int]
parse content = map (\c -> read c :: Int) (words content)

main :: IO ()
main = do
  input <- getContents

  ---putStrLn (show (parse input))
  putStrLn ("Part1: " ++ show ((part1 . parse) input))
  putStrLn ("Part2: " ++ show ((part1 . reorganizeToColumns . parse) input))
