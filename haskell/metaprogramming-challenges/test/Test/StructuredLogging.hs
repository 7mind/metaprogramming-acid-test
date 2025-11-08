{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.StructuredLogging (spec) where

import Test.Hspec
import Metaprogramming.StructuredLogging
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Aeson (toJSON)

spec :: Spec
spec = describe "Structured Logging" $ do
    describe "LogEntry" $ do
        it "creates a log entry with template and args" $ do
            let entry = LogEntry "Hello %user%" (Map.singleton "user" (toJSON ("John" :: T.Text)))
            template entry `shouldBe` "Hello %user%"
            Map.lookup "user" (args entry) `shouldBe` Just (toJSON ("John" :: T.Text))

        it "handles multiple arguments" $ do
            let entry = LogEntry
                    "User %name% is %age%"
                    (Map.fromList
                        [ ("name", toJSON ("Alice" :: T.Text))
                        , ("age", toJSON (30 :: Int))
                        ])
            Map.size (args entry) `shouldBe` 2

    describe "parseTemplate" $ do
        it "parses simple template" $ do
            parseTemplate "Hello {user}" `shouldBe` ("Hello %user%", ["user"])

        it "parses multiple placeholders" $ do
            parseTemplate "Hello {user}, balance: {balance}"
                `shouldBe` ("Hello %user%, balance: %balance%", ["user", "balance"])

        it "handles no placeholders" $ do
            parseTemplate "Hello world" `shouldBe` ("Hello world", [])
