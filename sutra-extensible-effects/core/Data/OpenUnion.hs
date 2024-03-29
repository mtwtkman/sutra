{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.OpenUnion (
  Union,
  inj,
  prj,
  pattern U0',
  decomp,
  pattern U0,
  pattern U1,
  Member,
  SetMember,
  type (<::),
  weaken,
) where

import Data.Kind (Type)
import Data.List (deleteFirstsBy)
import Data.Traversable (for)
import GHC.Base (Constraint)
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

{- | Explicit type -level equality condition is a deleteFirstsBy
hack to eliminate the type annotation in the trivial case,
such as @run (runReader () get)@

There is no ambiguity when finding instances for
@Member t (a ': b ': r)@, which the second instance is selected.

the only case we have to concerned about is @Member t '[s]@
But, in this case, values of definition is the same (if present),
and the first one is chosen according to GHC User Manual, since
the latter one is incoherent. This is the optimal choice.
-}
instance {-# OVERLAPPING #-} (t ~ s) => Member t '[s] where
  {-# INLINE inj #-}
  {-# INLINE prj #-}
  prj (Union _ x) = Just (unsafeCoerce x)
  inj = Union 0

-- Note that if it weren't for us wanting to use the specialized instance above
-- we wouldn't need the INCOHERENT pragma below
-- TODO: consider impact of disabling specialization
instance {-# INCOHERENT #-} (FindElem t r) => Member t r where
  {-# INLINE inj #-}
  {-# INLINE prj #-}
  inj = inj' (unP (elemNo :: P t r))
  prj = prj' (unP (elemNo :: P t r))

{- | A useful operator for reducing boilerplate in signatures.

The following lines are equivalent.

@
(Member (Exc e) r, Member (State s) r) => ...
[ Exc e, State s] <:: r =>
@
-}
type family (<::) (ms :: [Type -> Type]) r where
  (<::) '[] r = (() :: Constraint)
  (<::) (m ': ms) r = (Member m r, (<::) ms r)

{-# INLINE [2] decomp #-}

-- | Orthogonal decomposition of the union: head and the rest.
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (Union 0 v) = Right $ unsafeCoerce v
decomp (Union n v) = Left $ Union (n - 1) v

{- | Some helpful pattern synonyms.
U0 : the first element of the union
-}
pattern U0 :: t v -> Union (t ': r) v
pattern U0 h <- (decomp -> Right h)
  where
    U0 h = inj h

-- | U1 : everything excluding the first element of the union.
pattern U1 t <- (decomp -> Left t)
  where
    U1 t = weaken t

{-# COMPLETE U0, U1 #-}

-- Specialized version
{-# RULES "decomp/singleton" decomp = decomp0 #-}
{-# INLINE decomp0 #-}
decomp0 :: Union '[t] v -> Either (Union '[] v) (t v)
decomp0 (Union _ v) = Right $ unsafeCoerce v

weaken :: Union r w -> Union (any ': r) w
weaken (Union n v) = Union (n + 1) v

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

{- | Using overlapping instances here is OK since this class is private to this
module
-}
class EQU (a :: k) (b :: k) p | a b -> p

instance EQU a a 'True
instance {-# OVERLAPPABLE #-} (p ~ 'False) => EQU a b p

-- | This class is used for emulating monad transformers
class (Member t r) => SetMember (tag :: k -> Type -> Type) (t :: Type -> Type) r | tag r -> t

instance (EQU t1 t2 p, MemberU' p tag t1 (t2 ': r)) => SetMember tag t1 (t2 ': r)

class
  (Member t r) =>
  MemberU' (f :: Bool) (tag :: k -> Type -> Type) (t :: Type -> Type) r
    | tag r -> t

instance MemberU' 'True tag (tag e) (tag e ': r)
instance
  (Member t (t' ': r), SetMember tag t r) =>
  MemberU' 'False tag t (t' ': r)
