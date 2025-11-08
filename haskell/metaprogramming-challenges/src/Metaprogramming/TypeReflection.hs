{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Metaprogramming.TypeReflection
Description : Challenge 2 - Library-Based Reflection
Copyright   : (c) 2024
License     : MIT

This module implements type reflection using Haskell's Typeable class,
providing runtime type identity comparison and subtype checking.

Example:
@
let intId = typeId @Int
    strId = typeId @String
in isSame intId strId  -- False
@
-}
module Metaprogramming.TypeReflection
    ( TypeId
    , typeId
    , isSame
    , typeName
    -- Type class based "subtyping"
    , SubtypeOf(..)
    , isSubtypeOf
    , canCastTo
    ) where

import Data.Typeable (Typeable, cast)
import Type.Reflection (TypeRep, typeRep, eqTypeRep, (:~~:)(..), SomeTypeRep(..))
import Data.Kind (Type)

-- | Type identity wrapper that captures type information at runtime
data TypeId a where
    TypeId :: Typeable a => TypeRep a -> TypeId a

-- | Create a TypeId for a given type
-- Usage: typeId @Int
typeId :: forall a. Typeable a => TypeId a
typeId = TypeId (typeRep @a)

-- | Check if two TypeIds represent the same type
isSame :: TypeId a -> TypeId b -> Bool
isSame (TypeId rep1) (TypeId rep2) =
    case rep1 `eqTypeRep` rep2 of
        Just HRefl -> True
        Nothing -> False

-- | Get the name of a type from its TypeId
typeName :: TypeId a -> String
typeName (TypeId rep) = show rep

-- | Type class to represent subtype relationships
-- In Haskell, we model this through type classes rather than OOP inheritance
-- This represents "type A can be treated as type B in some context"
class SubtypeOf sub super where
    -- | Cast from subtype to supertype
    upcast :: sub -> super

-- | Check if one type is a subtype of another (via type class constraint)
-- This is a compile-time check, but we can use it at runtime with existentials
isSubtypeOf :: forall sub super. (Typeable sub, Typeable super, SubtypeOf sub super)
            => TypeId sub -> TypeId super -> Bool
isSubtypeOf _ _ = True  -- If the constraint is satisfied, it's a subtype

-- | Runtime check if a value can be cast to a target type
canCastTo :: forall a b. (Typeable a, Typeable b) => a -> TypeId b -> Maybe b
canCastTo val (TypeId targetRep) = cast val

-- Example instances showing subtype relationships
-- These would typically be defined by users of the library

-- Base example: Integers can be treated as Rationals
instance SubtypeOf Int Rational where
    upcast = fromIntegral

-- Lists are covariant in their element type (with additional constraints)
-- This is a simplified model - real subtyping is more complex in Haskell
instance SubtypeOf a b => SubtypeOf [a] [b] where
    upcast = map upcast

-- Maybe is covariant
instance SubtypeOf a b => SubtypeOf (Maybe a) (Maybe b) where
    upcast Nothing = Nothing
    upcast (Just x) = Just (upcast x)

-- Identity instance
instance SubtypeOf a a where
    upcast = id
