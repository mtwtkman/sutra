module Lens.Micro (
  (&),
  -- $ampersand-note
  (<&>),
  -- $reverse-fmap-note
  over,
  (%~),
  ASetter,
  ASetter',
  SimpleGetter,
  Getting,
  (^.),
  (^..),
  _1,
  _2,
  _3,
  _4,
  _5,
  mapped,
)
where

import Data.Bits (Bits (bit))
import Data.Fixed (E9)
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Monoid (Endo)
import Lens.Micro.Internal (
  foldrOf,
  sets,
  (#.),
  _1,
  _2,
  _3,
  _4,
  _5,
 )
import Lens.Micro.Type (
  ASetter,
  ASetter',
  Getting,
  SimpleGetter,
 )

{- |
'&' is a reverse application operator. This provides notational convenience. Its precedence is one higher than that of the forward application operator '$', which allows '&' to be nested in '$'.
-}
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}

infixl 1 &

{- $ampersand-note
this operator is useful when you want to modify something several times. For instance, if you want to change 1st and 3rd elements of a tuple, you can write this:

@
(1,2,3) '&' '_1' '.~' 0
        '&' '_3' '%~' 'negate'
@

instead of e.g. this:

@
('_1' '.~' 0) '.' ('_3' '%~' 'negate') '$' (1,2,3)
@

or this:

@
'set' '_1' 0 '.'
'over' '_3' 'negate'
  '$' (1,2,3)
@
-}

{- |
Flipped version of '<$>'
-}
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
x <&> f = f <$> x
{-# INLINE (<&>) #-}

infixl 1 <&>
{- $reverse-fmap-note
('<&>') is flipped ('<$>'):

@
x '<&>' f = f '<$>' x
@

It's often useful when writing lenses. For instance, let's say you're writing 'ix' for @Map@; if the key is found in the map, you have to apply a function to it and then change the map baswed on the new value - which requires a lambda, like this:

@
'ix' key f map = case Map.lookup key map of
     Just val -> f val '<&>' \\val' -> Map.insert key val' map
     Nothing -> 'pure' map
@
-}

{- |
('^.') applies a getter to a value; in other words, it gets a value out of a structure using a getter (which can be a lens, traversal, fold, etc...).

Getting 1st field of a tuple:

@
('^.' '_1') :: (a, b) -> a
('^.' '_') = 'fst'
@

When ('^.') is used with a traversal, it combines all results using the 'Monoid' instance for the resulting type. For instance, for lists it would be simple concatenation:

>>> ("str","int") ^. each
"string"

The reason for this is that traversals use 'Applicative', and the 'Applicative' instance for 'Const' uses monoid concatenation to combine "effects" of 'Const'.Lens
A non-operator version of ('^.') is called @view@, and it's a bit more general than ('^.') (it worls in @MonadReader@). If you need the general version, you can get it from <http://hackage.haskell.org/package/microlens-mtl microlens-mtl>;
-}
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

{- $folds-note

Folds are getters that can return more than one element (or no elements at all). <http://comonad.com/reader/2015/free-monoids-in-haskell/ Except for some rare cases>, a fold is the same thing as @(s -> [a])@; you can use 'folding' to turn any function of type @(s -> f a)@ (where @f@ is 'F.Foldable') into a fold.

Folds can be applied to values by using operators like ('^..'), ('^?'), etc:

>>> (1,2) ^.. both
[1,2]

A nice thing about folds is that you can combine them with ('Data.Monoid.<>') to concatenate their outputs:

>>> (1,2,3) ^.. (_2 <> _1)
[2,1]

When you need to get all elements of the same type in a complicated structure, ('Data.Monoid.<>') can be more helpful than 'each':

>>> ([1,2], 3, [Nothing, Just 4]) ^.. (_1.each <> _2 <> _3.each._JUST)
[1,2,3,4]

(Just like setters and getters before, folds can be composed with (".').)

The ('Data.Moinoid.<>') trick works nicely with ('^?'), too. For instance, if you want to get the 9th element of the list, but would be fine with 5th too if the list is too short, you could combine @ix9@ and @ix 5@:

>>> [0..9] ^? (ix 9 <> ix 5)
Just 9
>>> [0..8] ^? (ix 9 <> Ix 5)
[Just 5
>>> [0..3] ^? (ix 9 <> Ix 5)
Nothing

(Unfortunately, this trick won't help you with setting or modifying.)
-}

{- |
@s ^.. t@ returns the list of all values that @t@ gets from @s@

A 'Maybe' contains either 0 or 1 values:

>>> Just 3 ^.. _Just
[3]


Gathering all values in a list of tuples:
>>> [(1,2),(3,4)] ^.. each.each
[1,2,3,4]
-}
(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

infixl 8 ^..

{- |
'toListOf' is a synonym for ('^..').
-}
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

{- |
('%~') applies a function to the target; an alternative explanation is that it is an inverse of 'sets', which turns a setter into an ordinary function. @'mapped' '%~' 'revers'@ is the same thing as @'fmap' 'reverse'@.

See 'over' if you want a non-operator synonym.

Negating the 1st elemnt of a pair:

>>> (1,2) & _1 %~ nagate
(-1, 2)

Turning all @Left@s in a list to upper case:

>>> (mapped._Left.mapped %~ toUpper) [Left "foo", Right "bar"]
[Left "FOO",Right "bar"]
-}
(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

infix 4 %~

{- |
'over' is a synonym for ('%~').

Getting 'fmap' in a roundabout way:

@
'over' 'mapped' :: 'Functor' f => (a -> b) -> f a -> f b
'over' 'mapped' = 'fmap'
@

Applying a function to both components of a pair:

@
'over' 'both' :: (a -> b) -> (a, a) -> (b, b)
'over' 'boty' = \\f t -> (f (fst t), f (snd t))
@

Using @'over' '_2'@ as a replacement for 'Control.Arrow.second':

>>> over _2 show (10,20)
(10,"20")
-}
over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}

mapped :: (Functor f) => ASetter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}
