{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Metaprogramming.StructuredLogging
Description : Challenge 1 - Effortless Structured Logging
Copyright   : (c) 2024
License     : MIT

This module implements structured logging using Template Haskell to automatically
extract variable names and values from logging statements.

Example:
@
let user = "John"
    balance = 42
in $(logTH "Hello {user}, your balance is {balance}")
@

Outputs:
@
{
  "template": "Hello %user%, your balance is %balance%",
  "args": {
    "user": "John",
    "balance": 42
  }
}
@
-}
module Metaprogramming.StructuredLogging
    ( logTH
    , logQQ
    , logStr
    , LogEntry(..)
    ) where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Log entry containing template and arguments
data LogEntry = LogEntry
    { template :: Text
    , args :: Map Text Aeson.Value
    } deriving (Show, Eq)

instance ToJSON LogEntry where
    toJSON (LogEntry tmpl argMap) =
        object
            [ "template" .= tmpl
            , "args" .= argMap
            ]

-- | Parse a template string and extract variable placeholders
-- e.g., "Hello {user}" -> ("Hello %user%", ["user"])
parseTemplate :: String -> (String, [String])
parseTemplate str = go str [] []
  where
    go [] acc vars = (reverse acc, reverse vars)
    go ('{':rest) acc vars =
        let (varName, after) = span (/= '}') rest
        in case after of
            ('}':rest') -> go rest' (reverse ('%' : varName ++ "%") ++ acc) (varName : vars)
            _ -> error "Unclosed brace in template"
    go (c:rest) acc vars = go rest (c:acc) vars

-- | Template Haskell function to create structured logging
-- Usage: $(logTH "Hello {user}, balance: {balance}")
logTH :: String -> Q Exp
logTH templateStr = do
    let (newTemplate, varNames) = parseTemplate templateStr

    -- For each variable, create a lookup expression
    varExprs <- mapM (\name -> do
        -- Create variable name
        let varName = mkName name
        -- Create tuple: (name, toJSON var)
        [| (T.pack name, toJSON $(varE varName)) |]
        ) varNames

    -- Create the log entry
    [|
        let entry = LogEntry
                (T.pack newTemplate)
                (Map.fromList $(listE varExprs))
        in BSL.putStrLn (Aeson.encode entry)
       |]

-- | QuasiQuoter version for nicer syntax
-- Usage: [logQQ|Hello {user}, balance: {balance}|]
logQQ :: QuasiQuoter
logQQ = QuasiQuoter
    { quoteExp = logTH
    , quotePat = error "logQQ cannot be used in pattern context"
    , quoteType = error "logQQ cannot be used in type context"
    , quoteDec = error "logQQ cannot be used in declaration context"
    }

-- | Runtime logging function (doesn't capture variable names automatically)
-- This is provided for comparison - it requires explicit argument passing
logStr :: ToJSON a => String -> [(String, a)] -> IO ()
logStr templateStr pairs =
    let convertedPairs = [(T.pack k, toJSON v) | (k, v) <- pairs]
        entry = LogEntry (T.pack templateStr) (Map.fromList convertedPairs)
    in BSL.putStrLn (Aeson.encode entry)
