-- Day01.idr

module Day01

import Data.List
import Data.List1
import Data.String
import System
import System.File.ReadWrite

testf : String
testf = "data/day01-test.txt"

inputf : String
inputf = "data/day01-input.txt"

||| Read data from a file with a given name into a string
readData : String -> IO String
readData str = do
    Right contents <- readFile str | Left error => die ("### Error: " ++ show error)
    pure contents

formatData : String -> Maybe (List (List Nat))
formatData = traverse (traverse parseInteger) . splitIntoGroups
  where splitIntoGroups : String -> List (List String)
        splitIntoGroups = forget . split (== "") . lines

part1 : String -> Nat
part1 str = 0

||| Solve puzzle
main : IO ()
main = do
  contents <- readData testf
  putStrLn $ contents
