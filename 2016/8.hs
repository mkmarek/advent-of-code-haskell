import Data.Array (index)

main :: IO ()

type Screen = [[Bool]]

createScreen :: Int -> Int -> Screen
createScreen width height =
  replicate height (replicate width False)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

iteratePixels :: (Int -> Int -> Screen -> Bool) -> Screen -> Screen
iteratePixels it screen =
  mapWithIndex
    ( \y row ->
        mapWithIndex
          ( \x element ->
              it x y screen
          )
          row
    )
    screen

printScreen :: Screen -> String
printScreen screen =
  unlines
    ( map
        ( map
            ( \element ->
                if element
                  then '#'
                  else '.'
            )
        )
        screen
    )

rect :: Int -> Int -> Screen -> Screen
rect width height =
  iteratePixels
    ( \x y screen ->
        (x < width && y < height) || (screen !! y !! x)
    )

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow row amount =
  iteratePixels
    ( \x y screen ->
        if y == row
          then screen !! y !! ((x - amount) `mod` length (screen !! y))
          else screen !! y !! x
    )

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn column amount =
  iteratePixels
    ( \x y screen ->
        if x == column
          then screen !! ((y - amount) `mod` length screen) !! x
          else screen !! y !! x
    )

parseInstruction :: String -> Screen -> Screen
parseInstruction instruction screen =
  case words instruction of
    ["rect", size] ->
      let (width, height) = break (== 'x') size
       in rect (read width) (read (drop 1 height)) screen
    ["rotate", "row", row, "by", amount] ->
      case row of
        'y' : '=' : r -> rotateRow (read r) (read amount) screen
        _ -> error ("Invalid row " <> row)
    ["rotate", "column", column, "by", amount] ->
      case column of
        'x' : '=' : c -> rotateColumn (read c) (read amount) screen
        _ -> error ("Invalid column " <> column)
    _ -> screen

runInstructions :: [String] -> Screen -> Screen
runInstructions instructions screen =
  foldl (flip parseInstruction) screen instructions

countPixels :: Screen -> Int
countPixels screen =
  sum (map (length . filter id) screen)

main = do
  input <- getContents

  putStr "Part 1: \n"
  print (countPixels (runInstructions (lines input) (createScreen 50 6)))

  putStr "Part 2: \n"
  putStr (printScreen (runInstructions (lines input) (createScreen 50 6)))
