
import Data.Char (chr, ord, toLower, toUpper, isAscii)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.IO qualified as Text.Lazy.IO

hash :: Char -> Char -> Char
hash z c = chr $ (ord z + ord c) `mod` 128

encode :: String -> String -> IO ()
encode inputName outputName = do
  input <- readFile inputName
  let (p : removeFirstChar) = input
  let removedLastChar = init removeFirstChar
  let onlyAscii = filter isAscii removedLastChar
  let allUperCase = map toUpper onlyAscii
  let superHash =
        foldl
          ( \acc ci -> case acc of
              [] -> [hash p ci]
              acc@(di : _) -> acc ++ [hash di ci]
          )
          [p]
          (tail allUperCase)
  let allLowerCase = map toLower superHash
  writeFile outputName allLowerCase

encodeStrict :: String -> String -> IO ()
encodeStrict inputName outputName = do
  input <- Text.IO.readFile inputName
  let p = Text.head input
  let removeFirstChar = Text.tail input
  let removedLastChar = Text.init removeFirstChar
  let onlyAscii = Text.filter isAscii removedLastChar
  let allUperCase = Text.map toUpper onlyAscii
  let superHash = Text.scanl hash p (Text.tail allUperCase)
  let allLowerCase = Text.map toLower superHash
  Text.IO.writeFile outputName allLowerCase

encodeLazy :: String -> String -> IO ()
encodeLazy inputName outputName = do
  input <- Text.Lazy.IO.readFile inputName
  let p = Text.Lazy.head input
  let removeFirstChar = Text.Lazy.tail input
  let removedLastChar = Text.Lazy.init removeFirstChar
  let onlyAscii = Text.Lazy.filter isAscii removedLastChar
  let allUperCase = Text.Lazy.map toUpper onlyAscii
  let superHash = Text.Lazy.scanl hash p (Text.Lazy.tail allUperCase)
  let allLowerCase = Text.Lazy.map toLower superHash
  Text.Lazy.IO.writeFile outputName allLowerCase
