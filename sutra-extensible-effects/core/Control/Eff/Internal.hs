{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | A monadic library for communication between a handler and
its client, the administered computation

Original work available at <http://okmij.org/ftp/Haskell/extensible/tutorial.html>.
This module implements extensible effects as an alternative to monad transformers,
as described in <http://okmij.org/ftp/Haskell/extensible/exteff.pdf> and
<http://okmij.org/ftp/Haskell/extensible/more.pdf>.

Extensible Effects are implemented as typeclass constraints on an Eff[ect] datatype.
A contrived example can be found under "Control.Eff.Example". To run the
effects, consult the tests.
-}
module Control.Eff.Internal where

import qualified Control.Arrow as A
import qualified Control.Category as C
import Data.FTCQueue (FTCQueue (..), ViewL (..), tsingleton, tviewl, viewlMap, (><), (|>))
import Data.OpenUnion (Union)
import GHC.Exts (inline)

{- | Effectful arrow type: a function from a to b that also does effects
denoted by r
-}
type Arr r a b = a -> Eff r b

{- | An effectful function from @a@ to @b@ that is a composition of one or more
effectful funcitons. The parameter r describes the overall effect.

The composition members are accumulated in a type-aligned queue. Using a
newtype here enables us to define `C.Category` and `A.Arrow` instances.
-}
newtype Arrs r a b = Arrs (FTCQueue (Eff r) a b)

-- | 'Arrs' can be composed and have a natural identity.
instance C.Category (Arrs r) where
  id = ident
  f . g = comp g f

-- | As the name suggests, 'Arrs' also has an 'A.Arrow' instance.
instance A.Arrow (Arrs r) where
  arr = arr
  first = singleK . first . (^$)

first :: Arr r a b -> Arr r (a, c) (b, c)
first x (a, c) = (,c) `fmap` x a

{- | convert single effectfule arrow into composable type. i.e., convert 'Arr' to
'Arrs'
-}
{-# INLINE [2] singleK #-}
singleK :: Arr r a b -> Arrs r a b
singleK k = Arrs (tsingleton k)

{-# RULES
"singleK/qApp" [~2] forall q. singleK (qApp q) = q
  #-}

-- | Syntactic sugar for `qApp`
{-# INLINE [2] (^$) #-}
(^$) :: forall r b w. Arrs r b w -> b -> Eff r w
(^$) = qApp

infixl 9 ^$

-- | Lift a function to an arrow
arr :: (a -> b) -> Arrs r a b
arr f = singleK (Val . f)

-- | The identity arrow
ident :: Arrs r a a
ident = arr id

-- | Arrow composition
comp :: Arrs r a b -> Arrs r b c -> Arrs r a c
comp (Arrs f) (Arrs g) = Arrs (f >< g)

-- | Common pattern: append 'Arr' to 'Arrs'
(^|>) :: Arrs r a b -> Arr r b c -> Arrs r a c
(Arrs f) ^|> g = Arrs (f |> g)

infixl 9 ^|>

{- | The monad that all effects in this library are based on.

An effectful computation is a value of type `Eff r a`.
In this signature, @r@ is a type-level list of effects that are being
requested and need to be handled inside an effectful computation.
@a@ is the computation's result similar to other monads.

A computation's result can be retrieved via the 'run' function.
However, all effects used in the computation need to be hadnled by the use
of the effects' @run*@ functions before unwrapping the final result.
For additional details, see the documentation of the effects you are using.
-}
data Eff r a
  = Val !a
  | forall b. E !(Arrs r b a) !(Union r b)

{- | Application to the `generalized effectful function' @Arrs r bw @, i.e.,
convert 'Arrs' to 'Arr'
-}
{-# INLINEABLE [2] qApp #-}
qApp :: forall r b w. Arrs r b w -> Arr r b w
qApp (Arrs q) x = viewlMap (inline tviewl q) ($ x) cons
 where
  cons :: forall x. Arr r b x -> FTCQueue (Eff r) x w -> Eff r w
  cons k t = case k x of
    Val y -> qApp (Arrs t) y
    E (Arrs q0) u -> E (Arrs (q0 >< t)) u

{- | Case analysis for 'Eff' dtatype. If the value is @'Val' a@ apply
the first function to @a@; if it is @'E' u q@, apply the second
function.
-}
{-# INLINE eff #-}
eff ::
  (a -> b) ->
  (forall v. Arrs r v a -> Union r v -> b) ->
  Eff r a ->
  b
eff f _ (Val a) = f a
eff _ g (E q u) = g q u

{- | The usual 'bind' function with arguments flipped. This is a
common pattern for Eff.
-}
bind :: Arr r a b -> Eff r a -> Eff r b
bind k = eff k (E . (^|> k)) -- just accumulates continuations

instance Functor (Eff r) where
  {-# INLINE fmap #-}
  fmap f = bind (Val . f)
