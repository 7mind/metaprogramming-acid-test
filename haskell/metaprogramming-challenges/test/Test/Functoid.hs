{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Functoid (spec) where

import Test.Hspec
import Metaprogramming.Functoid

spec :: Spec
spec = describe "Functoid" $ do
    describe "Unary functoid" $ do
        let double :: Int -> Int
            double x = x * 2

            doubleFunctoid = functoid1 @"value" double

        it "has correct arity" $
            arity doubleFunctoid `shouldBe` 1

        it "has correct param types" $
            paramTypeNames doubleFunctoid `shouldBe` ["Int"]

        it "has correct param IDs" $
            paramIds doubleFunctoid `shouldBe` [Just "value"]

        it "can be invoked" $
            (invoke doubleFunctoid (21 :: Int) :: Int) `shouldBe` 42

    describe "Binary functoid" $ do
        let add :: Int -> Int -> Int
            add x y = x + y

            addFunctoid = functoid2 @"left" @"right" add

        it "has correct arity" $
            arity addFunctoid `shouldBe` 2

        it "has correct param types" $
            paramTypeNames addFunctoid `shouldBe` ["Int", "Int"]

        it "has correct param IDs" $
            paramIds addFunctoid `shouldBe` [Just "left", Just "right"]

        it "can be invoked" $
            (invoke addFunctoid (10 :: Int) (32 :: Int) :: Int) `shouldBe` 42

    describe "Ternary functoid" $ do
        let combine :: String -> Int -> Bool -> String
            combine name age active =
                name ++ ":" ++ show age ++ ":" ++ show active

            combineFunctoid = functoid3 @"name" @"age" @"active" combine

        it "has correct arity" $
            arity combineFunctoid `shouldBe` 3

        it "has correct param types" $
            paramTypeNames combineFunctoid `shouldBe` ["[Char]", "Int", "Bool"]

        it "has correct param IDs" $
            paramIds combineFunctoid `shouldBe` [Just "name", Just "age", Just "active"]

        it "can be invoked" $
            (invoke combineFunctoid "Alice" (30 :: Int) True :: String) `shouldBe` "Alice:30:True"

    describe "paramInfo" $ do
        let greet :: String -> String
            greet name = "Hello " ++ name

            greetFunctoid = functoid1 @"user-name" greet

        it "combines type and ID information" $ do
            let info = paramInfo greetFunctoid
            length info `shouldBe` 1
            paramType (head info) `shouldBe` "[Char]"
            paramId (head info) `shouldBe` Just "user-name"
