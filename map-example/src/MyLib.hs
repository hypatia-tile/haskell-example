module MyLib (someFunc) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

fruits :: [(String, Int)]
fruits = [("apple", 1), ("orange", 2), ("banana", 3), ("peach", 4), ("cherry", 5), ("orange", 6), ("apple", 7), ("peach", 8)]

fruitsMap = M.fromList fruits

someFunc :: IO ()
someFunc = putStrLn "someFunc"
