{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.TypeReflection (spec) where

import Test.Hspec
import Metaprogramming.TypeReflection

spec :: Spec
spec = describe "Type Reflection" $ do
    describe "isSame" $ do
        it "returns True for same types" $ do
            let id1 = typeId @Int
                id2 = typeId @Int
            isSame id1 id2 `shouldBe` True

        it "returns False for different types" $ do
            let intId = typeId @Int
                strId = typeId @String
            isSame intId strId `shouldBe` False

    describe "typeName" $ do
        it "returns type name for Int" $ do
            typeName (typeId @Int) `shouldBe` "Int"

        it "returns type name for String" $ do
            typeName (typeId @String) `shouldBe` "[Char]"

        it "returns type name for complex types" $ do
            typeName (typeId @[Int]) `shouldBe` "[Int]"

    describe "SubtypeOf" $ do
        it "upcasts Int to Rational" $ do
            let val = 42 :: Int
                result = upcast val :: Rational
            result `shouldBe` 42

        it "supports list covariance" $ do
            let intList = [1, 2, 3] :: [Int]
                ratList = upcast intList :: [Rational]
            ratList `shouldBe` [1, 2, 3]

    describe "canCastTo" $ do
        it "successfully casts to same type" $ do
            let val = 42 :: Int
            canCastTo val (typeId @Int) `shouldBe` Just 42

        it "fails to cast to different type" $ do
            let val = 42 :: Int
            canCastTo val (typeId @String) `shouldBe` Nothing
