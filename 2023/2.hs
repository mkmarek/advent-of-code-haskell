{-# LANGUAGE OverloadedRecordDot #-}

import Data.Text (Text, empty, pack, splitOn, unpack)

data Cube = Cube
  { color :: Text,
    amount :: Int
  }
  deriving (Show)

data Game = Game
  { number :: Int,
    sets :: [[Cube]]
  }
  deriving (Show)

getAmountByColor :: [Cube] -> Text -> Int
getAmountByColor cubes col =
  case filter (\c -> color c == col) cubes of
    [] -> 0
    [c] -> amount c

parseInput :: [Text] -> [Game]
parseInput = map parseGame

parseSet :: Text -> [Cube]
parseSet input = map parseCube (splitOn (pack ",") input)

parseCube :: Text -> Cube
parseCube input =
  let [amount, color] = filter (/= empty) (splitOn (pack " ") input)
   in Cube {color = color, amount = read (unpack amount)}

parseGame :: Text -> Game
parseGame input =
  let [left, rest] = splitOn (pack ":") input
      sets = map parseSet (splitOn (pack ";") rest)
   in Game {number = read (unpack (splitOn (pack " ") left !! 1)), sets = sets}

isGamePossible :: [Cube] -> Game -> Bool
isGamePossible cubes game =
  let s = sets game
   in all (isSetPossible cubes) s

isSetPossible :: [Cube] -> [Cube] -> Bool
isSetPossible cubes set =
  let colors = map (\c -> (color c, amount c)) set
      amounts = map (\(col, am) -> getAmountByColor cubes col - am) colors
   in all (>= 0) amounts

areGamesPossible :: [Cube] -> [Game] -> Int
areGamesPossible cubes game = sum (map (\g -> g.number) (filter (isGamePossible cubes) game))

findMinimumSetOfCubes :: Game -> Int
findMinimumSetOfCubes game =
  let minGreen = maximum (map (\s -> getAmountByColor s (pack "green")) (sets game))
      minRed = maximum (map (\s -> getAmountByColor s (pack "red")) (sets game))
      minBlue = maximum (map (\s -> getAmountByColor s (pack "blue")) (sets game))
   in minGreen * minRed * minBlue

main = do
  input <- getContents

  putStrLn
    ( "Part 1: "
        <> show
          ( areGamesPossible
              [ Cube {color = pack "red", amount = 12},
                Cube {color = pack "green", amount = 13},
                Cube {color = pack "blue", amount = 14}
              ]
              (parseInput (map pack (lines input)))
          )
    )

  putStrLn
    ("Part 2: " <> show (sum (map findMinimumSetOfCubes (parseInput (map pack (lines input))))))
