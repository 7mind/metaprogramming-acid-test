module Main (main) where

import Test.Hspec

import qualified Test.StructuredLogging
import qualified Test.TypeReflection
import qualified Test.Functoid

main :: IO ()
main = hspec $ do
    describe "Metaprogramming Challenges" $ do
        Test.StructuredLogging.spec
        Test.TypeReflection.spec
        Test.Functoid.spec
