module Lens.Micro.Extras () where

import Data.Functor.Const (Const (Const, getConst))
import Lens.Micro.Internal ((#.))
import Lens.Micro.Type (Getting)

{- |
'view' is a synonym for ('^.'):

>>> view _1 (1, 2)
1

The reason it's not in "Lens.Micro" is  that @view@ in lens has a more general signature:

@
view :: MonadReader s m => Getting a s a -> m a
@

So, you would be able to use this 'view' with functions, but not in various reader monads. For mots people this shoudn't be an issue; if it is for you, use @view@ from <http://hackage.haskell.org/package/microlens-mtl microlens-mtl>.
-}
view :: Getting a s a -> s -> a
view l = getConst #. l Const
{-# INLINE view #-}
