{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

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

    functoid = functoid2 @"name" @"age" greet

in do
    print $ arity functoid                    -- 2
    print $ paramTypeNames functoid           -- ["String", "Int"]
    print $ returnTypeName functoid           -- "String"
    print $ invoke functoid "Alice" 30        -- "Hello Alice, you are 30"
@
-}
module Metaprogramming.Functoid
    ( Functoid
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
import Type.Reflection (TypeRep, typeRep, SomeTypeRep(..), pattern App)
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

-- | Decompose a function type into parameter types and return type
-- For example: (a -> b -> c) becomes ([a, b], c)
decomposeFunctionType :: TypeRep f -> ([SomeTypeRep], SomeTypeRep)
decomposeFunctionType tr = go (SomeTypeRep tr) []
  where
    go :: SomeTypeRep -> [SomeTypeRep] -> ([SomeTypeRep], SomeTypeRep)
    go someRep acc = case someRep of
      -- Match function type: ((->) a b) is represented as App (App arrow a) b
      SomeTypeRep (App (App _arrow param) ret) ->
        -- Recursively decompose the return type
        go (SomeTypeRep ret) (SomeTypeRep param : acc)
      -- Not a function type, return accumulated params and this as return type
      _ -> (reverse acc, someRep)

-- | Get the arity (number of parameters) of a Functoid
arity :: Functoid func -> Int
arity (Functoid _ (_ :: func)) = length $ fst $ decomposeFunctionType (typeRep @func)

-- | Get parameter type names
paramTypeNames :: forall func. Functoid func -> [String]
paramTypeNames (Functoid _ (_ :: func)) =
  let (params, _) = decomposeFunctionType (typeRep @func)
  in map show params

-- | Get parameter IDs
paramIds :: Functoid func -> [Maybe String]
paramIds (Functoid ids _) = ids

-- | Get parameter info (type + ID)
paramInfo :: Functoid func -> [ParamInfo]
paramInfo functoid = zipWith ParamInfo (paramTypeNames functoid) (paramIds functoid)

-- | Get return type name
returnTypeName :: forall func. Typeable func => Functoid func -> String
returnTypeName (Functoid _ (_ :: func)) =
  let (_, ret) = decomposeFunctionType (typeRep @func)
  in show ret

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

-- | Create a unary functoid with parameter ID
-- Usage: functoid1 @"userId" myFunction
functoid1 :: forall id a r. (KnownSymbol id, Typeable a, Typeable r)
          => (a -> r) -> Functoid (a -> r)
functoid1 f = Functoid [Just $ symbolVal (Proxy @id)] f

-- | Create a binary functoid with parameter IDs
-- Usage: functoid2 @"name" @"age" myFunction
functoid2 :: forall id1 id2 a b r.
             (KnownSymbol id1, KnownSymbol id2, Typeable a, Typeable b, Typeable r)
          => (a -> b -> r) -> Functoid (a -> b -> r)
functoid2 f = Functoid
    [ Just $ symbolVal (Proxy @id1)
    , Just $ symbolVal (Proxy @id2)
    ] f

-- | Create a ternary functoid with parameter IDs
-- Usage: functoid3 @"x" @"y" @"z" myFunction
functoid3 :: forall id1 id2 id3 a b c r.
             (KnownSymbol id1, KnownSymbol id2, KnownSymbol id3,
              Typeable a, Typeable b, Typeable c, Typeable r)
          => (a -> b -> c -> r) -> Functoid (a -> b -> c -> r)
functoid3 f = Functoid
    [ Just $ symbolVal (Proxy @id1)
    , Just $ symbolVal (Proxy @id2)
    , Just $ symbolVal (Proxy @id3)
    ] f
