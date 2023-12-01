import Data.ByteString qualified as B
import Data.Char (isAlpha)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Distribution.Utils.MD5
import GHC.Base (ord)

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

findNextLetter :: String -> Int -> (Int, Char, Char, String)
findNextLetter str n =
  let hash = show (md5 (packStr'' (str ++ show n)))
   in case hash of
        '0' : '0' : '0' : '0' : '0' : c1 : c2 : rest -> (n, c1, c2, rest)
        _ -> findNextLetter str (n + 1)

findPassword :: String -> Int -> Int -> String
findPassword input depth n =
  if depth < 8
    then
      let (n', c, _, _) = findNextLetter input n
       in c : findPassword input (depth + 1) (n' + 1)
    else ""

replaceAt :: Int -> Char -> String -> String
replaceAt pos newChar s =
  let (start, _ : rest) = splitAt pos s
   in start ++ [newChar] ++ rest

replaceIfUnderscore :: String -> Char -> Int -> String
replaceIfUnderscore s c i =
  if s !! i == '_'
    then replaceAt i c s
    else s

findPassword2 :: String -> String -> Int -> String
findPassword2 input acc n =
  if '_' `elem` acc
    then
      let (n', c, c2, rest) = findNextLetter input n
       in if ord c - 48 >= 0 && ord c - 48 <= 7
            then findPassword2 input (replaceIfUnderscore acc c2 (ord c - 48)) (n' + 1)
            else findPassword2 input acc (n' + 1)
    else acc

main :: IO ()
main = do
  input <- getContents

  print ("Part 1: " ++ findPassword (filter isAlpha input) 0 0)
  print ("Part 2: " ++ findPassword2 (filter isAlpha input) "________" 0)
