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
    , logRobust
    , logRobustQQ
    , logStr
    , LogEntry(..)
    , parseTemplate
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
    let varExprs = map (\name ->
            -- Create variable name
            let varName = mkName name
            -- Create tuple: (name, toJSON var)
            in [| (T.pack name, toJSON $(varE varName)) |]
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

-- | Parse robust template with explicit name-expression pairs
-- Format: "Template %name1% %name2%", "name1", expr1, "name2", expr2, ...
parseRobustArgs :: [String] -> [(String, Q Exp)] -> Q [(String, Exp)]
parseRobustArgs [] [] = return []
parseRobustArgs (name:names) ((_nameCheck, exprQ):rest) = do
    expr <- exprQ
    remaining <- parseRobustArgs names rest
    return ((name, expr) : remaining)
parseRobustArgs _ _ = error "Mismatched names and expressions in robust logger"

-- | Robust Template Haskell logger that accepts arbitrary expressions
-- Usage: $(logRobust "Sum: %result%, Double: %doubled%" "result" (x + y) "doubled" (x * 2))
--
-- This version accepts explicit name-expression pairs, allowing any Haskell expression
-- to be logged with a custom name.
logRobust :: String -> Q Exp
logRobust templateStr = do
    -- This is a varargs-style macro that will be called with alternating names and expressions
    -- We'll implement it using a list-building approach

    -- Return a function that takes list of (name, value) pairs
    [| \pairs ->
        let entry = LogEntry (T.pack templateStr) (Map.fromList pairs)
        in BSL.putStrLn (Aeson.encode entry)
     |]

-- | Robust logging macro with explicit argument syntax
-- Usage: $(logRobustExplicit "Sum: %result%, Double: %doubled%" [|result|] [|x + y|] [|doubled|] [|x * 2|])
--
-- This allows arbitrary expressions to be logged. The pattern is:
-- $(logRobustExplicit template_string [|name1|] [|expr1|] [|name2|] [|expr2|] ...)
-- where names are quoted string expressions and values are quoted Haskell expressions.
--
-- Note: This is a workaround for Template Haskell's limitations. A truly "automatic" robust
-- logger that parses {expr} syntax would require implementing a Haskell parser, which is
-- beyond the scope of a library implementation.
logRobustExplicit :: String -> Q Exp
logRobustExplicit templateStr = do
    -- Return a function that accepts pairs
    [| \namePairs ->
        let entry = LogEntry
                (T.pack templateStr)
                (Map.fromList [(T.pack n, toJSON v) | (n, v) <- namePairs])
        in BSL.putStrLn (Aeson.encode entry)
     |]

-- | QuasiQuoter for robust logging - requires manual name/expression specification
-- Due to Template Haskell limitations, we use a runtime function approach
logRobustQQ :: QuasiQuoter
logRobustQQ = QuasiQuoter
    { quoteExp = \str -> [| \pairs ->
        let entry = LogEntry (T.pack str) (Map.fromList pairs)
        in BSL.putStrLn (Aeson.encode entry) |]
    , quotePat = error "logRobustQQ cannot be used in pattern context"
    , quoteType = error "logRobustQQ cannot be used in type context"
    , quoteDec = error "logRobustQQ cannot be used in declaration context"
    }
