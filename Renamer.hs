module Main where

import System.Environment
import System.Directory
import System.IO

import Text.Regex

import Data.Maybe
import Data.Char

-------------
-- Globals --
-------------
globalWidth :: Int
globalWidth = 3

----------
-- Pure --
----------
replace :: String -> String -> String -> String
replace old new str = subRegex (mkRegex old) str new

exists :: String -> String -> Bool
exists regex str = matchRegex (mkRegex regex) str /= Nothing

toLowerString :: String -> String
toLowerString str = map (toLower) str

isDigitString :: String -> Bool
isDigitString [] = True
isDigitString (x:xs) = isDigit x && isDigitString xs

gen0sRaw :: Int -> Int -> String
gen0sRaw cap curr
  | curr < cap = '0' : gen0sRaw cap (curr + 1)
  | otherwise = ""

gen0s :: Int -> String
gen0s cap = gen0sRaw cap 0

numerate :: String -> Int -> String
numerate str width
  | desiredWidth <= 0 = str
  | otherwise = (gen0s desiredWidth) ++ str
  where desiredWidth = width - (length str)

extractEulerNumber :: String -> String
extractEulerNumber str =
  replace "(euler|.hs)" "" lowered
  where lowered = toLowerString str

isEulerString :: String -> Bool
isEulerString str =
  (exists "euler" lowered) && (exists ".hs" lowered) && (isDigitString $ extractEulerNumber str)
  where lowered = toLowerString str

fixEulerString :: String -> String
fixEulerString str
  | isEulerString str = "Euler" ++ (numerate (extractEulerNumber str) globalWidth) ++ ".hs"
  | otherwise = str

------------
-- Impure --
------------
copy :: FilePath -> FilePath -> IO ()
copy src dest = (readFile src) >>= (writeFile dest)

fixAll :: [FilePath] -> IO ()
fixAll [] = return ()
fixAll (x:xs)
  | isEulerString x = do
    copy x $ fixEulerString x
    removeFile x
    fixAll xs
  | otherwise = fixAll xs

main = do
  args <- getArgs

  if length args == 1
    then do
      setCurrentDirectory $ args !! 0
      contents <- (getCurrentDirectory >>= getDirectoryContents)
      fixAll contents
    else putStrLn "Proper usage: renamer.exe <directory name>"