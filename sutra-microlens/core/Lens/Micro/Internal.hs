{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Lens.Micro.Internal (
  traversed,
  sets,
  Field1 (..),
  Field2 (..),
  Field3 (..),
  Field4 (..),
  Field5 (..),
  (#.),
  (.#),
  foldrOf,
  foldMapOf,
) where

import Control.Applicative (Const (Const, getConst))
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Monoid (Endo (Endo, appEndo))
import Lens.Micro.Type (ASetter, Getting, Lens, Traversal)

{- |
'traversed' traverses any 'Traversable' container (list, vector, @Map@, 'Maybe', you name it):

>>> Just 1 ^.. traversed
[1]

'traversed' is the same as 'traverse', but can be faster thanks to magic rewrite rules.
-}
traversed :: (Traversable f) => Traversal (f a) (f b) a b
traversed = traverse
{-# INLINE [0] traversed #-}

{- |
'sets' creates an 'ASetter' from an ordinary function. (The only thing it does is wrapping and unwrapping 'Identity'.)
-}
sets :: ((a -> b) -> s -> t) -> ASetter s t a b
sets f g = Identity #. f (runIdentity #. g)
{-# INLINE sets #-}

{- |
  Gives access to the 1st field of a tuple (up to 5-tuples).

  Getting the 1st component:

  >>> (1,2,3,4,5) ^. _1
  1

  Setting the 1st component:

  >>> (1,2,3) & _1 .~ 10
  (10,2,3)

  Note that this lens is lazy, and can set fields even of 'undefined':

  >>> set _1 10 undefined :: (Int, Int)
  (10, *** Excepiton: Prelude.undefined

  This is done to avoid violating a lens law stating that you can get back what you put:

  >>> view _1 . set _1 10 $ (undefined :: (Int, Int))
  10

  The implementation (for 2-tuples) is:

  @
  '_1' f t = (,) '<$>' f    ('fst' t)
               '<*>' 'pure' ('snd' t)
  @

  or, alternatively,

  @
  '_1' f ~(a,b) = (\\a' -> (a',b)) '<$>' f a
  @

  (where @~@ means a <https://wiki.haskell.org/Lazy_pattern_match lazy pattern>);

  '_2', '_3', '_4', and '_5' are also available (see below).
-}
class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: Lens s t a b

instance Field1 (a, b) (a', b) a a' where
  _1 k ~(a, b) = (,b) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a, b, c) (a', b, c) a a' where
  _1 k (a, b, c) = (,b,c) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a, b, c, d) (a', b, c, d) a a' where
  _1 k (a, b, c, d) = (,b,c,d) <$> k a
  {-# INLINE _1 #-}

instance Field1 (a, b, c, d, e) (a', b, c, d, e) a a' where
  _1 k (a, b, c, d, e) = (,b,c,d,e) <$> k a
  {-# INLINE _1 #-}

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b

instance Field2 (a, b) (a, b') b b' where
  _2 k ~(a, b) = (a,) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a, b, c) (a, b', c) b b' where
  _2 k (a, b, c) = (a,,c) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a, b, c, d) (a, b', c, d) b b' where
  _2 k (a, b, c, d) = (a,,c,d) <$> k b
  {-# INLINE _2 #-}

instance Field2 (a, b, c, d, e) (a, b', c, d, e) b b' where
  _2 k (a, b, c, d, e) = (a,,c,d,e) <$> k b
  {-# INLINE _2 #-}

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b

instance Field3 (a, b, c) (a, b, c') c c' where
  _3 k ~(a, b, c) = (a,b,) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a, b, c, d) (a, b, c', d) c c' where
  _3 k ~(a, b, c, d) = (a,b,,d) <$> k c
  {-# INLINE _3 #-}

instance Field3 (a, b, c, d, e) (a, b, c', d, e) c c' where
  _3 k ~(a, b, c, d, e) = (a,b,,d,e) <$> k c
  {-# INLINE _3 #-}

class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _4 :: Lens s t a b

instance Field4 (a, b, c, d) (a, b, c, d') d d' where
  _4 k ~(a, b, c, d) = (a,b,c,) <$> k d
  {-# INLINE _4 #-}

instance Field4 (a, b, c, d, e) (a, b, c, d', e) d d' where
  _4 k ~(a, b, c, d, e) = (a,b,c,,e) <$> k d
  {-# INLINE _4 #-}

class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _5 :: Lens s t a b

instance Field5 (a, b, c, d, e) (a, b, c, d, e') e e' where
  _5 k ~(a, b, c, d, e) = (a,b,c,d,) <$> k e
  {-# INLINE _5 #-}

------------------------------------------------------------------
-- Coerce-like composition
------------------------------------------------------------------

-- Note: 'lens' defines a type-restricted version of (#.) to work around a
-- bug, but our version is restricted enough that we don't need it. See
-- <https://github.com/ekmett/lens/commit/cde2fc39c0dba413d1a6f814b47bd14431a5e339>

(#.) :: (Coercible c b) => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce (\x -> x :: b) :: forall a b. (Coercible b a) => a -> b
(.#) :: (Coercible b a) => (b -> c) -> (a -> b) -> (a -> c)
(.#) pbc _ = coerce pbc
{-# INLINE (#.) #-}
{-# INLINE (.#) #-}

infix 9 #.
infix 8 .#

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}
