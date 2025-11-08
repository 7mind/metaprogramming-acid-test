{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Metaprogramming.TypeReflection

main :: IO ()
main = do
    putStrLn "=== Type Reflection Examples ==="

    putStrLn "\n=== Example 1: Type identity comparison ==="
    let intId = typeId @Int
        strId = typeId @String
        intId2 = typeId @Int
    putStrLn $ "Int == String: " ++ show (isSame intId strId)
    putStrLn $ "Int == Int: " ++ show (isSame intId intId2)

    putStrLn "\n=== Example 2: Type names ==="
    putStrLn $ "Type name of Int: " ++ typeName intId
    putStrLn $ "Type name of String: " ++ typeName strId
    putStrLn $ "Type name of [Int]: " ++ typeName (typeId @[Int])
    putStrLn $ "Type name of Maybe Bool: " ++ typeName (typeId @(Maybe Bool))

    putStrLn "\n=== Example 3: Type fingerprints ==="
    putStrLn $ "Fingerprint of Int: " ++ show (typeFingerprint intId)
    putStrLn $ "Fingerprint of String: " ++ show (typeFingerprint strId)

    putStrLn "\n=== Example 4: Subtype checking via type classes ==="
    let intVal = 42 :: Int
        rationalId = typeId @Rational
    putStrLn $ "Int value: " ++ show intVal
    putStrLn $ "Upcast to Rational: " ++ show (upcast intVal :: Rational)

    putStrLn "\n=== Example 5: Runtime casting ==="
    case canCastTo intVal (typeId @Int) of
        Just val -> putStrLn $ "Successfully cast to Int: " ++ show val
        Nothing -> putStrLn "Cast failed"

    case canCastTo intVal (typeId @String) of
        Just val -> putStrLn $ "Successfully cast to String: " ++ show val
        Nothing -> putStrLn "Cast to String failed (as expected)"

    putStrLn "\n=== Example 6: List subtyping ==="
    let intList = [1, 2, 3] :: [Int]
        rationalList = upcast intList :: [Rational]
    putStrLn $ "Int list: " ++ show intList
    putStrLn $ "Rational list: " ++ show rationalList

    putStrLn "\n=== All examples completed! ==="
