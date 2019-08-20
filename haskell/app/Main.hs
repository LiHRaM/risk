module Main where

import Lib

main :: IO ()
main = do
    battle <- newBattle
    putStrLn $ show battle
    main' battle
    return ()

main' :: Battle -> IO Battle
main' b = do
    if canAttack b then do
        should <- shouldAttack
        if should then do
            battle <- attack b
            putStrLn $ show battle
            main' battle
        else return b
    else return b
