{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Metaprogramming.StructuredLogging
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "=== Structured Logging Examples ==="

    putStrLn "\n=== Example 1: Basic logging ==="
    let user = "John" :: T.Text
        balance = 42 :: Int
    $(logTH "Hello {user}, your balance is {balance}")

    putStrLn "\n=== Example 2: Multiple types ==="
    let name = "Alice" :: T.Text
        age = 30 :: Int
        score = 95.5 :: Double
        active = True :: Bool
    $(logTH "User {name} is {age} years old with score {score} (active: {active})")

    putStrLn "\n=== Example 3: Nested data ==="
    let productName = "Widget" :: T.Text
        quantity = 5 :: Int
        price = 29.99 :: Double
    $(logTH "Product {productName}: {quantity} units at ${price} each")

    putStrLn "\n=== Example 4: Complex expressions ==="
    let x = 10 :: Int
        y = 20 :: Int
        total = x + y
    $(logTH "Sum of {x} and {y} is {total}")

    putStrLn "\n=== All examples completed! ==="
