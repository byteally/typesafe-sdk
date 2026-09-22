{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : TypeSafe.Internal.Generic
-- Description : Generic enumeration of nullary constructors
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- Support code for the default methods of 'TypeSafe.Question.ChoiceOption'
-- and 'TypeSafe.Question.ScoreLevel'. It is exposed so that the constraints of
-- those defaults can be named in user code; its contents may change between
-- minor versions.
module TypeSafe.Internal.Generic
  ( GEnumerate (..)
  , GConstructorName (..)
  , genericEnumerate
  , genericConstructorName
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | Types whose generic representation is a sum of constructors without
-- fields.
class GEnumerate (f :: Type -> Type) where
  -- | Every value, in declaration order.
  genumerate :: [f p]

instance GEnumerate U1 where
  genumerate = [U1]

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :+: g) where
  genumerate = map L1 genumerate <> map R1 genumerate

instance (GEnumerate f) => GEnumerate (D1 c f) where
  genumerate = map M1 genumerate

instance (GEnumerate f) => GEnumerate (C1 c f) where
  genumerate = map M1 genumerate

instance
  ( TypeError
      ( 'Text "Cannot derive the options of a type whose constructors have fields."
          ':$$: 'Text "Choice options and Score levels are derived for enumerations such as"
          ':$$: 'Text "  data Department = Billing | Technical | Sales"
          ':$$: 'Text "Implement the class methods by hand, or use choiceBy / scoreBy."
      )
  ) =>
  GEnumerate (S1 c f)
  where
  genumerate = []

instance
  ( TypeError
      ( 'Text "Cannot derive the options of a type without constructors."
          ':$$: 'Text "A Choice needs at least one option and a Score at least one level."
      )
  ) =>
  GEnumerate V1
  where
  genumerate = []

-- | Every constructor of an enumeration, in declaration order.
genericEnumerate :: (Generic a, GEnumerate (Rep a)) => NonEmpty a
genericEnumerate = case map to genumerate of
  x : xs -> x :| xs
  -- Unreachable: types without constructors are rejected by the V1 instance.
  [] -> error "TypeSafe.Internal.Generic.genericEnumerate: no constructors"

-- | Types whose generic representation lets us name the constructor.
class GConstructorName (f :: Type -> Type) where
  gconstructorName :: f p -> String

instance (GConstructorName f) => GConstructorName (D1 c f) where
  gconstructorName (M1 x) = gconstructorName x

instance (GConstructorName f, GConstructorName g) => GConstructorName (f :+: g) where
  gconstructorName (L1 x) = gconstructorName x
  gconstructorName (R1 x) = gconstructorName x

instance (Constructor c) => GConstructorName (C1 c f) where
  gconstructorName = conName

-- | The name of the constructor used to build a value.
genericConstructorName :: (Generic a, GConstructorName (Rep a)) => a -> String
genericConstructorName = gconstructorName . from
