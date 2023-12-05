data Range = Range
  { destinationRangeStart :: Int,
    sourceRangeStart :: Int,
    rangeLength :: Int
  }
  deriving (Show)

data Maps = Maps
  { seedToSoil :: [Range],
    soilToFertilizer :: [Range],
    fertilizerToWater :: [Range],
    waterToLight :: [Range],
    lightToTemperature :: [Range],
    temperatureToHumidity :: [Range],
    humidityToLocation :: [Range]
  }
  deriving (Show)

data Input = Input
  { seeds :: [(Int, Int)],
    maps :: Maps
  }
  deriving (Show)

parseRange :: String -> Range
parseRange s =
  let w = words s
   in Range (read (head w)) (read (w !! 1)) (read (w !! 2))

combineRanges :: [Range] -> [Range] -> [Range]
combineRanges [] [] = []
combineRanges [] (r : rs) = r : combineRanges [] rs
combineRanges (r : rs) [] = r : combineRanges rs []
combineRanges (r1 : rs1) (r2 : rs2)
  | destinationRangeStart r1 < destinationRangeStart r2 =
      r1 : combineRanges rs1 (r2 : rs2)
  | destinationRangeStart r1 > destinationRangeStart r2 =
      r2 : combineRanges (r1 : rs1) rs2
  | otherwise =
      Range
        (destinationRangeStart r1)
        (min (sourceRangeStart r1) (sourceRangeStart r2))
        (max (rangeLength r1) (rangeLength r2))
        : combineRanges rs1 rs2

combineMaps :: Maps -> Maps -> Maps
combineMaps m1 m2 =
  Maps
    { seedToSoil = combineRanges (seedToSoil m1) (seedToSoil m2),
      soilToFertilizer = combineRanges (soilToFertilizer m1) (soilToFertilizer m2),
      fertilizerToWater = combineRanges (fertilizerToWater m1) (fertilizerToWater m2),
      waterToLight = combineRanges (waterToLight m1) (waterToLight m2),
      lightToTemperature = combineRanges (lightToTemperature m1) (lightToTemperature m2),
      temperatureToHumidity = combineRanges (temperatureToHumidity m1) (temperatureToHumidity m2),
      humidityToLocation = combineRanges (humidityToLocation m1) (humidityToLocation m2)
    }

parseMaps :: [String] -> Maps
parseMaps lines =
  case lines of
    [] -> Maps [] [] [] [] [] [] []
    "seed-to-soil map:" : rest ->
      let (ranges, rest') = break (== "") rest
       in combineMaps (Maps (map parseRange ranges) [] [] [] [] [] []) (parseMaps (tail rest'))
    "soil-to-fertilizer map:" : rest ->
      let (ranges, rest') = break (== "") rest
       in combineMaps (Maps [] (map parseRange ranges) [] [] [] [] []) (parseMaps (tail rest'))
    "fertilizer-to-water map:" : rest ->
      let (ranges, rest') = break (== "") rest
       in combineMaps (Maps [] [] (map parseRange ranges) [] [] [] []) (parseMaps (tail rest'))
    "water-to-light map:" : rest ->
      let (ranges, rest') = break (== "") rest
       in combineMaps (Maps [] [] [] (map parseRange ranges) [] [] []) (parseMaps (tail rest'))
    "light-to-temperature map:" : rest ->
      let (ranges, rest') = break (== "") rest
       in combineMaps (Maps [] [] [] [] (map parseRange ranges) [] []) (parseMaps (tail rest'))
    "temperature-to-humidity map:" : rest ->
      let (ranges, rest') = break (== "") rest
       in combineMaps (Maps [] [] [] [] [] (map parseRange ranges) []) (parseMaps (tail rest'))
    "humidity-to-location map:" : rest ->
      let (ranges, rest') = break (== "") rest
       in combineMaps (Maps [] [] [] [] [] [] (map parseRange ranges)) (parseMaps rest')
    _ -> error ("Invalid input:" <> show lines)

overlapRanges :: (Int, Int) -> Range -> ([(Int, Int)], [(Int, Int)])
overlapRanges (start, end) range =
  let destination = destinationRangeStart range
      source = sourceRangeStart range
      length = rangeLength range
   in if start >= source && end <= source + length -- completely inside range
        then ([(destination + start - source, destination + end - source)], [])
        else
          if start < source && end <= source + length && end >= source -- overlaps begin of range
            then ([(destination, destination + end - source)], [(start, source - 1)])
            else
              if start >= source && start <= source + length && end > source + length -- overlaps end of range
                then ([(destination + start - source, destination + length)], [(source + length + 1, end)])
                else
                  if start < source && end > source + length -- overlaps both begin and end of range
                    then ([(destination, destination + length)], [(start, source - 1), (source + length + 1, end)])
                    else ([], [(start, end)]) -- completely outside range

convertWithRanges :: [Range] -> (Int, Int) -> [(Int, Int)]
convertWithRanges ranges value =
  let (mapped, unmapped) =
        foldl
          ( \(mapped, unmapped) range ->
              let result = map (`overlapRanges` range) unmapped
               in (mapped ++ concatMap fst result, concatMap snd result)
          )
          ([], [value])
          ranges
   in mapped ++ unmapped

convertSeedsWithRanges :: [Range] -> [(Int, Int)] -> [(Int, Int)]
convertSeedsWithRanges ranges values =
  case values of
    [] -> []
    v : vs -> convertSeedsWithRanges ranges vs ++ convertWithRanges ranges v

convertSeeds :: Input -> [(Int, Int)]
convertSeeds input =
  foldl
    (flip convertSeedsWithRanges)
    (seeds input)
    [seedToSoil (maps input), soilToFertilizer (maps input), fertilizerToWater (maps input), waterToLight (maps input), lightToTemperature (maps input), temperatureToHumidity (maps input), humidityToLocation (maps input)]

parseSeedsForPart1 :: [Int] -> [(Int, Int)]
parseSeedsForPart1 seeds =
  case seeds of
    [] -> []
    s : ss -> parseSeedsForPart1 ss ++ [(s, s)]

parseSeedsForPart2 :: [Int] -> [(Int, Int)]
parseSeedsForPart2 seeds =
  case seeds of
    [] -> []
    s1 : s2 : ss -> parseSeedsForPart2 ss ++ [(s1, s1 + s2)]

parseInput :: [String] -> ([Int] -> [(Int, Int)]) -> Input
parseInput lines parseSeeds =
  let (firstLine, rest) = break (== "") lines
   in case head lines of
        's' : 'e' : 'e' : 'd' : 's' : ':' : s ->
          let seeds = words s
           in Input (parseSeeds (map read seeds)) (parseMaps (tail rest))
        _ -> error "Invalid input"

main = do
  input <- getContents

  putStrLn ("Part 1: " <> show (minimum (convertSeeds (parseInput (lines input) parseSeedsForPart1))))
  putStrLn ("Part 2: " <> show (minimum (convertSeeds (parseInput (lines input) parseSeedsForPart2))))
