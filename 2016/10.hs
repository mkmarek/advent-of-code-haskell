import Data.List (intersect)

trackValue :: Int -> [String] -> (Int, Int)
trackValue value lines =
  case lines of
    [] -> (-1, -1)
    line : rest ->
      case words line of
        ["value", val, "goes", "to", "bot", botNumber] ->
          if (read val :: Int) == value
            then trackBot (read botNumber) rest
            else trackValue value rest
        _ -> trackValue value rest

trackBot :: Int -> [String] -> (Int, Int)
trackBot botIndex lines =
  case lines of
    [] -> (botIndex, 0)
    line : rest ->
      case words line of
        ["bot", sourceBot, "gives", "low", "to", "bot", lowBot, "and", "high", "to", "bot", highBot] ->
          ( if ((read lowBot :: Int) == botIndex) || ((read highBot :: Int) == botIndex)
              then trackBot (read sourceBot) rest
              else trackBot botIndex rest
          )
        _ -> trackBot botIndex rest

trackBotForward :: Int -> [String] -> [Int]
trackBotForward botIndex lines =
  case lines of
    [] -> [botIndex]
    line : rest ->
      case words line of
        ["bot", sourceBot, "gives", "low", "to", "bot", lowBot, "and", "high", "to", "bot", highBot] ->
          ( if (read sourceBot :: Int) == botIndex
              then (botIndex : (read lowBot :: Int) : trackBotForward (read lowBot) rest) <> ((read highBot :: Int) : trackBotForward (read highBot) rest)
              else trackBotForward botIndex rest
          )
        _ -> trackBotForward botIndex rest

main = do
  input <- getContents

  putStr "\nPart 1: \n"
  let (leftBotIndex, leftRestLines) = trackValue 61 (reverse (lines input))
      left = trackBotForward leftBotIndex (drop leftRestLines (lines input))
      (rightBotIndex, rightRestLines) = trackValue 17 (reverse (lines input))
      right = trackBotForward rightBotIndex (drop rightRestLines (lines input))
  putStr (show left)
  putStr "\n"
  putStr (show right)
  putStr "\n"
  putStr (show (left `intersect` right))

  putStr "\nPart 2: \n"
  putStr "\n"
