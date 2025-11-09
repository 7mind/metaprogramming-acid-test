{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Metaprogramming.Functoid
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "=== Functoid Examples ==="

    putStrLn "\n=== Example 1: Unary function with ID ==="
    let greet :: String -> String
        greet name = "Hello, " ++ name

        greetFunctoid = functoid1 @"user-name" greet

    putStrLn $ "Arity: " ++ show (arity greetFunctoid)
    putStrLn $ "Param types: " ++ show (paramTypeNames greetFunctoid)
    putStrLn $ "Param IDs: " ++ show (paramIds greetFunctoid)
    putStrLn $ "Return type: " ++ show (returnTypeName greetFunctoid)
    putStrLn $ "Invoke: " ++ invoke greetFunctoid "Alice"

    putStrLn "\n=== Example 2: Binary function with IDs ==="
    let add :: Int -> Int -> Int
        add x y = x + y

        addFunctoid = functoid2 @"left" @"right" add

    putStrLn $ "Arity: " ++ show (arity addFunctoid)
    putStrLn $ "Param types: " ++ show (paramTypeNames addFunctoid)
    putStrLn $ "Param IDs: " ++ show (paramIds addFunctoid)
    putStrLn $ "Param info: " ++ show (paramInfo addFunctoid)
    putStrLn $ "Return type: " ++ show (returnTypeName addFunctoid)
    putStrLn $ "Invoke: " ++ show ((invoke addFunctoid (10 :: Int) (20 :: Int)) :: Int)

    putStrLn "\n=== Example 3: Ternary function with IDs ==="
    let combine :: String -> Int -> Bool -> String
        combine name age isActive =
            name ++ " is " ++ show age ++
            (if isActive then " (active)" else " (inactive)")

        combineFunctoid = functoid3 @"name" @"age" @"active" combine

    putStrLn $ "Arity: " ++ show (arity combineFunctoid)
    putStrLn $ "Param types: " ++ show (paramTypeNames combineFunctoid)
    putStrLn $ "Param IDs: " ++ show (paramIds combineFunctoid)
    putStrLn $ "Return type: " ++ show (returnTypeName combineFunctoid)
    putStrLn $ "Invoke: " ++ (invoke combineFunctoid "Bob" (25 :: Int) True :: String)

    putStrLn "\n=== Example 4: Function with complex types ==="
    let processData :: [Int] -> Maybe String -> (Int, String)
        processData nums maybeStr =
            let total = sum nums
                str = maybe "none" id maybeStr
            in (total, str)

        processFunctoid = functoid2 @"numbers" @"label" processData

    putStrLn $ "Arity: " ++ show (arity processFunctoid)
    putStrLn $ "Param types: " ++ show (paramTypeNames processFunctoid)
    putStrLn $ "Param IDs: " ++ show (paramIds processFunctoid)
    putStrLn $ "Return type: " ++ show (returnTypeName processFunctoid)
    putStrLn $ "Invoke: " ++ show ((invoke processFunctoid [1,2,3,4,5] (Just "sum")) :: (Int, String))

    putStrLn "\n=== All examples completed! ==="
