import Data.Char
import Data.List (sort, sortBy)
import Data.Map qualified

data Room = Room {hash :: String, sectorId :: Int, checksum :: String, calculatedChecksum :: String}
  deriving (Show)

parse :: String -> [Room]
parse input = map parseRoom (words input)

parseRoom :: String -> Room
parseRoom str =
  let hash = filter (/= '-') (takeWhile isCharOrDash str)
      sectorId = read ((takeWhile isNumber . dropWhile isCharOrDash) str) :: Int
      checkSum = filter (\c -> c /= '[' && c /= ']') (dropWhile (/= '[') str)
   in Room hash sectorId checkSum ((take 5 . calculateChecksum) hash)

isCharOrDash :: Char -> Bool
isCharOrDash c
  | isAlpha c = True
  | c == '-' = True
  | otherwise = False

calculateChecksum :: String -> String
calculateChecksum str =
  (reverse . map fst)
    ( sortBy
        (\(k1, a) (k2, b) -> if a == b then compare k2 k1 else compare a b)
        (Data.Map.foldlWithKey (\a c v -> (c, v) : a) [] (countLetterOccurences str))
    )

countLetterOccurences :: String -> Data.Map.Map Char Int
countLetterOccurences = foldr (\c m -> Data.Map.insertWith (+) c 1 m) Data.Map.empty

checkSumMatches :: Room -> Bool
checkSumMatches r =
  case r of
    Room _ _ checksum calculatedChecksum -> checksum == calculatedChecksum

rotateLetters :: String -> Int -> String
rotateLetters str n =
  map
    ( \c ->
        let c' = ord c + mod n 26
         in if c' > 122 then chr (c' - 26) else chr c'
    )
    str

main :: IO ()
main = do
  input <- getContents

  print ("Part1: " ++ show ((sum . map sectorId . filter checkSumMatches) (parse input)))
  print
    ( "Part2: "
        ++ show
          ( ( map sectorId
                . filter (\r -> rotateLetters (hash r) (sectorId r) == "northpoleobjectstorage")
                . filter checkSumMatches
            )
              (parse input)
          )
    )
