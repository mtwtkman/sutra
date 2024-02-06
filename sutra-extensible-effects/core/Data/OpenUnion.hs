{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.OpenUnion (
  Union,
  inj,
  prj,
  pattern U0',
  Member,
) where

import Data.Kind (Type)
import GHC.TypeLits (
  ErrorMessage (
    ShowType,
    Text,
    (:$$:),
    (:<>:)
  ),
  TypeError,
 )
import Unsafe.Coerce (unsafeCoerce)

{- | The data constructors of Union are not exported

Strong Sum (Existential with the evidence) is an open union
t is can be a GADT and hence not necessarily a Functor.
Int is the index of t in the list r; that is, the index of t in the
universe r
-}
data Union (r :: [Type -> Type]) v where
  Union :: {-# UNPACK #-} !Int -> t v -> Union r v

{-# INLINE prj' #-}
{-# INLINE inj' #-}
inj' :: Int -> t v -> Union r v
inj' = Union

prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x)
  | n == n' = Just (unsafeCoerce x)
  | otherwise = Nothing

newtype P t r = P {unP :: Int}

{- | Typeclass that asserts that effect @t@ is contained inside the effect-list
@r@.

The @FindElem@ typeclass is an implementation detail and not required for
using the effect list or implementing custom effects.
-}
class (FindElem t r) => Member (t :: Type -> Type) r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

-- | Pattern synonym to project the union onto the effect @t@.
pattern U0' :: (Member t r) => t v -> Union r v
pattern U0' h <- (prj -> Just h)
  where
    U0' h = inj h

{- | Find the index of an element in a type-level list.
The element must exist
This is essentially a compile-time computation.
Using overlapping instances here is OK since this class is private to this
module
-}
class FindElem (t :: Type -> Type) r where
  elemNo :: P t r

instance FindElem t (t ': r) where
  elemNo = P 0

instance {-# OVERLAPPABLE #-} (FindElem t r) => FindElem t (t' ': r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)

instance
  ( TypeError
      ( 'Text "Cannot unify effect types."
          ':$$: 'Text "Unhandled effect: "
            ':<>: 'ShowType t
          ':$$: 'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?"
      )
  ) =>
  FindElem t '[]
  where
  elemNo = error "unreachable"
