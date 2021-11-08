module Main where

calculateFuelRec :: Int -> Int
calculateFuelRec m
  | fuel <= 0 = 0
  | otherwise = fuel + calculateFuelRec fuel
  where fuel = div m 3 - 2

main :: IO ()
main = do
  rawInputs <- readFile "./input-1.txt"
  let massess = map (calculateFuelRec . (read @Int)) $ lines rawInputs
  print $ sum massess
