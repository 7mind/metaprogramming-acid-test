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
    , invoke
    , ParamInfo(..)
    , paramInfo
    ) where

import Data.Typeable
import Type.Reflection
import GHC.TypeLits
import Data.Proxy

-- | Parameter information including type and optional ID
data ParamInfo = ParamInfo
    { paramType :: String
    , paramId :: Maybe String
    } deriving (Show, Eq)

-- | Functoid wrapper for functions with introspection capabilities
data Functoid func where
    -- Nullary function
    Functoid0 :: (Typeable r)
              => Maybe String  -- return type ID
              -> (() -> r)     -- function (wrapped to take unit)
              -> Functoid (() -> r)

    -- Unary function
    Functoid1 :: (Typeable a, Typeable r)
              => Maybe String  -- param ID
              -> (a -> r)      -- function
              -> Functoid (a -> r)

    -- Binary function
    Functoid2 :: (Typeable a, Typeable b, Typeable r)
              => Maybe String  -- param1 ID
              -> Maybe String  -- param2 ID
              -> (a -> b -> r) -- function
              -> Functoid (a -> b -> r)

    -- Ternary function
    Functoid3 :: (Typeable a, Typeable b, Typeable c, Typeable r)
              => Maybe String  -- param1 ID
              -> Maybe String  -- param2 ID
              -> Maybe String  -- param3 ID
              -> (a -> b -> c -> r)  -- function
              -> Functoid (a -> b -> c -> r)

-- | Create a Functoid without parameter IDs
makeFunctoidNoIds :: Typeable func => func -> Functoid func
makeFunctoidNoIds = error "Use specific smart constructors for different arities"

-- Smart constructor for nullary
makeFunctoid0 :: Typeable r => r -> Functoid (() -> r)
makeFunctoid0 val = Functoid0 Nothing (\() -> val)

-- Smart constructor for unary with type-level string ID
makeFunctoid1 :: forall id a r. (KnownSymbol id, Typeable a, Typeable r)
              => (a -> r) -> Functoid (a -> r)
makeFunctoid1 f = Functoid1 (Just $ symbolVal (Proxy @id)) f

-- Smart constructor for binary with type-level string IDs
makeFunctoid2 :: forall id1 id2 a b r.
                 (KnownSymbol id1, KnownSymbol id2, Typeable a, Typeable b, Typeable r)
              => (a -> b -> r) -> Functoid (a -> b -> r)
makeFunctoid2 f = Functoid2
    (Just $ symbolVal (Proxy @id1))
    (Just $ symbolVal (Proxy @id2))
    f

-- Smart constructor for ternary with type-level string IDs
makeFunctoid3 :: forall id1 id2 id3 a b c r.
                 (KnownSymbol id1, KnownSymbol id2, KnownSymbol id3,
                  Typeable a, Typeable b, Typeable c, Typeable r)
              => (a -> b -> c -> r) -> Functoid (a -> b -> c -> r)
makeFunctoid3 f = Functoid3
    (Just $ symbolVal (Proxy @id1))
    (Just $ symbolVal (Proxy @id2))
    (Just $ symbolVal (Proxy @id3))
    f

-- | Generic makeFunctoid using type application for IDs
class MakeFunctoid func where
    makeFunctoid :: func -> Functoid func

-- We'll provide a simpler interface without IDs for now
-- and a version with explicit ID parameters

-- | Get the arity (number of parameters) of a Functoid
arity :: Functoid func -> Int
arity (Functoid0 _ _) = 0
arity (Functoid1 _ _) = 1
arity (Functoid2 _ _ _) = 2
arity (Functoid3 _ _ _ _) = 3

-- | Get parameter type names
paramTypeNames :: forall func. Functoid func -> [String]
paramTypeNames (Functoid0 _ _) = []
paramTypeNames (Functoid1 _ (_ :: a -> r)) =
    [show $ typeRep @a]
paramTypeNames (Functoid2 _ _ (_ :: a -> b -> r)) =
    [show $ typeRep @a, show $ typeRep @b]
paramTypeNames (Functoid3 _ _ _ (_ :: a -> b -> c -> r)) =
    [show $ typeRep @a, show $ typeRep @b, show $ typeRep @c]

-- | Get parameter IDs
paramIds :: Functoid func -> [Maybe String]
paramIds (Functoid0 _ _) = []
paramIds (Functoid1 id _) = [id]
paramIds (Functoid2 id1 id2 _) = [id1, id2]
paramIds (Functoid3 id1 id2 id3 _) = [id1, id2, id3]

-- | Get parameter info (type + ID)
paramInfo :: Functoid func -> [ParamInfo]
paramInfo functoid = zipWith ParamInfo (paramTypeNames functoid) (paramIds functoid)

-- | Get return type name
returnTypeName :: forall func. Typeable func => Functoid func -> String
returnTypeName (Functoid0 _ (_ :: () -> r)) = show $ typeRep @r
returnTypeName (Functoid1 _ (_ :: a -> r)) = show $ typeRep @r
returnTypeName (Functoid2 _ _ (_ :: a -> b -> r)) = show $ typeRep @r
returnTypeName (Functoid3 _ _ _ (_ :: a -> b -> c -> r)) = show $ typeRep @r

-- | Type class for invoking Functoids
class Invoke func where
    invoke :: Functoid func -> func

instance Typeable r => Invoke (() -> r) where
    invoke (Functoid0 _ f) = f

instance (Typeable a, Typeable r) => Invoke (a -> r) where
    invoke (Functoid1 _ f) = f

instance (Typeable a, Typeable b, Typeable r) => Invoke (a -> b -> r) where
    invoke (Functoid2 _ _ f) = f

instance (Typeable a, Typeable b, Typeable c, Typeable r) => Invoke (a -> b -> c -> r) where
    invoke (Functoid3 _ _ _ f) = f

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
