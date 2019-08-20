{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( newBattle
    , canAttack
    , shouldAttack
    , attack
    , Battle
    ) where

import Data.List (sortBy)
import Control.Monad (replicateM)
import Control.Monad.Random (RandomGen, Rand, getRandomR, evalRandIO)

data Battle = Battle { attacker :: Int
                     , defender :: Int 
                     } deriving Show

data Rolls = Rolls { items :: [Int]
                   } deriving Show

getNumber :: IO Int
getNumber = readLn

getRoll :: (RandomGen g) => Rand g Int
getRoll = getRandomR (1,6)

getRolls :: (RandomGen g) => Int -> Rand g [Int]
getRolls n = Control.Monad.replicateM n getRoll
-- e.g. values <- evalRandIO (dice 2)

shouldAttack :: IO Bool
shouldAttack = do
    putStrLn "Attack? (y/n)"
    attack <- getLine
    case attack of
        "y" -> return True
        _ -> return False

newBattle :: IO Battle
newBattle = do
    putStrLn "Size of attacker's army: "
    attacker <- readLn
    putStrLn "Size of defender's army: "
    defender <- readLn
    return Battle { attacker
                    , defender
                    }

canAttack :: Battle -> Bool
canAttack b = attacker b > 0 && defender b > 0

attack :: Battle -> IO Battle
attack b = do
    attacker_rolls <- evalRandIO (getRolls (min 3 (attacker b)))
    defender_rolls <- evalRandIO (getRolls (min 3 (defender b)))
    let att_rolls_sorted = sortBy (flip compare) attacker_rolls
    let def_rolls_sorted = sortBy (flip compare) defender_rolls
    return $ attack' b att_rolls_sorted def_rolls_sorted

attack' :: Battle -> [Int] -> [Int] -> Battle
attack' b [] d  = b
attack' b a  [] = b
attack' b [a] [d] = attack'' b (a > d)
attack' b (a:as) (d:ds) = attack' battle as ds
    where battle = attack'' b (a > d)

attack'' :: Battle -> Bool -> Battle
attack'' b s
    | s == True = Battle { attacker = (attacker b), defender = (defender b) - 1 }
    | otherwise = Battle { attacker = (attacker b) - 1, defender = (defender b) }