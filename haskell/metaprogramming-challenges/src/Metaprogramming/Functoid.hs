{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Metaprogramming.Functoid
Description : Challenge 3 - Functoid Concept
Copyright   : (c) 2024
License     : MIT

This module implements the Functoid concept - wrapping functions with runtime
introspection capabilities to retrieve parameter type information and invoke
with dynamically-provided arguments.

Example:
@
let greet :: String -> Int -> String
    greet name age = "Hello " ++ name ++ ", you are " ++ show age

    functoid = makeFunctoid @"name" @"age" greet

in do
    print $ arity functoid                    -- 2
    print $ paramTypeNames functoid           -- ["String", "Int"]
    print $ returnTypeName functoid           -- "String"
    print $ invoke functoid "Alice" 30        -- "Hello Alice, you are 30"
@
-}
module Metaprogramming.Functoid
    ( Functoid
    , makeFunctoid
    , makeFunctoidNoIds
    , arity
    , paramTypeNames
    , paramIds
    , returnTypeName
    , Invoke(..)
    , ParamInfo(..)
    , paramInfo
    , functoid1
    , functoid2
    , functoid3
    ) where

import Data.Typeable (Typeable)
import Type.Reflection (TypeRep, typeRep)
import GHC.TypeLits
import Data.Proxy

-- | Parameter information including type and optional ID
data ParamInfo = ParamInfo
    { paramType :: String
    , paramId :: Maybe String
    } deriving (Show, Eq)

-- | Functoid wrapper for functions with introspection capabilities
data Functoid func where
    Functoid :: Typeable func
             => [Maybe String]  -- parameter IDs
             -> func            -- the function
             -> Functoid func

-- | Create a Functoid without parameter IDs
makeFunctoidNoIds :: Typeable func => func -> Functoid func
makeFunctoidNoIds f = Functoid [] f

-- Smart constructor for nullary
makeFunctoid0 :: Typeable r => r -> Functoid (() -> r)
makeFunctoid0 val = Functoid [] (\() -> val)

-- Smart constructor for unary with type-level string ID
makeFunctoid1 :: forall id a r. (KnownSymbol id, Typeable a, Typeable r)
              => (a -> r) -> Functoid (a -> r)
makeFunctoid1 f = Functoid [Just $ symbolVal (Proxy @id)] f

-- Smart constructor for binary with type-level string IDs
makeFunctoid2 :: forall id1 id2 a b r.
                 (KnownSymbol id1, KnownSymbol id2, Typeable a, Typeable b, Typeable r)
              => (a -> b -> r) -> Functoid (a -> b -> r)
makeFunctoid2 f = Functoid
    [ Just $ symbolVal (Proxy @id1)
    , Just $ symbolVal (Proxy @id2)
    ] f

-- Smart constructor for ternary with type-level string IDs
makeFunctoid3 :: forall id1 id2 id3 a b c r.
                 (KnownSymbol id1, KnownSymbol id2, KnownSymbol id3,
                  Typeable a, Typeable b, Typeable c, Typeable r)
              => (a -> b -> c -> r) -> Functoid (a -> b -> c -> r)
makeFunctoid3 f = Functoid
    [ Just $ symbolVal (Proxy @id1)
    , Just $ symbolVal (Proxy @id2)
    , Just $ symbolVal (Proxy @id3)
    ] f

-- We'll provide a simpler interface without IDs for now
-- and a version with explicit ID parameters

-- Helper function to decompose function types using string parsing
-- Parses the string representation of a TypeRep to extract function components
decomposeFunctionType :: TypeRep f -> ([String], String)
decomposeFunctionType tr = parseType (show tr)
  where
    -- Parse "a -> b -> c" into (["a", "b"], "c")
    parseType :: String -> ([String], String)
    parseType s =
      let parts = splitOn " -> " s
      in if length parts > 1
         then (init parts, last parts)
         else ([], s)

    -- Simple split function
    splitOn :: String -> String -> [String]
    splitOn delim str = go str []
      where
        go [] acc = [reverse acc]
        go s@(c:cs) acc =
          case stripPrefix delim s of
            Just rest -> reverse acc : go rest []
            Nothing -> go cs (c : acc)

    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] ys = Just ys
    stripPrefix _ [] = Nothing
    stripPrefix (x:xs) (y:ys)
      | x == y = stripPrefix xs ys
      | otherwise = Nothing

-- | Get the arity (number of parameters) of a Functoid
arity :: Functoid func -> Int
arity (Functoid _ (_ :: func)) = length $ fst $ decomposeFunctionType (typeRep @func)

-- | Get parameter type names
paramTypeNames :: forall func. Functoid func -> [String]
paramTypeNames (Functoid _ (_ :: func)) = fst $ decomposeFunctionType (typeRep @func)

-- | Get parameter IDs
paramIds :: Functoid func -> [Maybe String]
paramIds (Functoid ids _) = ids

-- | Get parameter info (type + ID)
paramInfo :: Functoid func -> [ParamInfo]
paramInfo functoid = zipWith ParamInfo (paramTypeNames functoid) (paramIds functoid)

-- | Get return type name
returnTypeName :: forall func. Typeable func => Functoid func -> String
returnTypeName (Functoid _ (_ :: func)) = snd $ decomposeFunctionType (typeRep @func)

-- | Type class for invoking Functoids
class Invoke func where
    invoke :: Functoid func -> func

instance Invoke (() -> r) where
    invoke (Functoid _ f) = f

instance {-# OVERLAPPING #-} Invoke (a -> b -> c -> r) where
    invoke (Functoid _ f) = f

instance {-# OVERLAPPING #-} Invoke (a -> b -> r) where
    invoke (Functoid _ f) = f

instance {-# OVERLAPPABLE #-} Invoke (a -> r) where
    invoke (Functoid _ f) = f

-- Helper functions to create functoids with IDs via type applications
-- These provide a nicer interface

-- | Create a unary functoid with parameter ID
-- Usage: functoid1 @"userId" myFunction
functoid1 :: forall id a r. (KnownSymbol id, Typeable a, Typeable r)
          => (a -> r) -> Functoid (a -> r)
functoid1 = makeFunctoid1 @id

-- | Create a binary functoid with parameter IDs
-- Usage: functoid2 @"name" @"age" myFunction
functoid2 :: forall id1 id2 a b r.
             (KnownSymbol id1, KnownSymbol id2, Typeable a, Typeable b, Typeable r)
          => (a -> b -> r) -> Functoid (a -> b -> r)
functoid2 = makeFunctoid2 @id1 @id2

-- | Create a ternary functoid with parameter IDs
-- Usage: functoid3 @"x" @"y" @"z" myFunction
functoid3 :: forall id1 id2 id3 a b c r.
             (KnownSymbol id1, KnownSymbol id2, KnownSymbol id3,
              Typeable a, Typeable b, Typeable c, Typeable r)
          => (a -> b -> c -> r) -> Functoid (a -> b -> c -> r)
functoid3 = makeFunctoid3 @id1 @id2 @id3
