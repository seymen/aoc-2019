{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Array
import           Data.List.Split
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

data Machine = Machine {
                         getMemory  :: Array Int Int
                       , getPointer :: Int
                       , hasHalted  :: Bool
                       } deriving (Show)

data InstructionRunResult = InstructionRunResult {
                                                   getElem0 :: Int
                                                 , getPair  :: (Int, Int)
                                                 } deriving (Show)

step :: Machine -> Machine
step machine@(Machine mem pointer _) =
  case mem ! pointer of
    1 -> let a = mem ! (mem ! (pointer + 1))
             b = mem ! (mem ! (pointer + 2))
             dest = mem ! (pointer + 3)
         in machine {
                      getMemory = mem // [(dest, a+b)]
                    , getPointer = pointer + 4
                    }
    2 -> let a = mem ! (mem ! (pointer + 1))
             b = mem ! (mem ! (pointer + 2))
             dest = mem ! (pointer + 3)
         in machine {
                      getMemory = mem // [(dest, a*b)]
                    , getPointer = pointer + 4
                    }
    99 -> machine {hasHalted = True}
    _  -> error "invalid input"

machineFromFile :: FilePath -> IO Machine
machineFromFile path = do
  raw <- T.readFile path
  let xs = map (read @Int . T.unpack) $ T.splitOn "," $ T.strip raw
  let arr = listArray (0, length xs - 1) xs
  return $ Machine arr 0 False

executeMachineUntilHalt :: Machine -> Machine
executeMachineUntilHalt = head . dropWhile (not . hasHalted) . iterate step

prepMachine :: Machine -> (Int, Int) -> Machine
prepMachine machine@(Machine mem _ _) (noun, verb) =
  machine { getMemory = mem // [(1, noun), (2, verb)]}

output :: Machine -> Int
output = (! 0) . getMemory

main :: IO ()
main = do
  -- part 1
  machine <- machineFromFile "./input-1.txt"
  print $ output $ executeMachineUntilHalt $ prepMachine machine (12, 2)
  -- part 2
  machine <- machineFromFile "./input-1.txt"
  print $ head $
    [ (noun, verb)
    | noun <- [0..99]
    , verb <- [0..99]
    , output (executeMachineUntilHalt $ prepMachine machine (noun, verb)) == 19690720
    ]
