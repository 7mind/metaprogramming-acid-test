{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Metaprogramming.StructuredLogging
import Metaprogramming.TypeReflection
import Metaprogramming.Functoid
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "=== All Metaprogramming Challenges ==="

    -- Challenge 1: Structured Logging
    putStrLn "\n=== Challenge 1: Structured Logging ==="
    let user = "John" :: T.Text
        balance = 42 :: Int
    $(logTH "User {user} has balance {balance}")

    -- Challenge 2: Type Reflection
    putStrLn "\n=== Challenge 2: Type Reflection ==="
    let intId = typeId @Int
        strId = typeId @String
    putStrLn $ "Int == String: " ++ show (isSame intId strId)
    putStrLn $ "Type name: " ++ typeName intId
    putStrLn $ "Upcast 42 :: Int to Rational: " ++ show (upcast (42 :: Int) :: Rational)

    -- Challenge 3: Functoid
    putStrLn "\n=== Challenge 3: Functoid ==="
    let greet :: String -> String -> String
        greet msg name = msg ++ ", " ++ name

        functoid = functoid2 @"greeting" @"user" greet

    putStrLn $ "Arity: " ++ show (arity functoid)
    putStrLn $ "Param info: " ++ show (paramInfo functoid)
    putStrLn $ "Invoke: " ++ invoke functoid "Hello" "World"

    putStrLn "\n=== All challenges demonstrated! ==="
