{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lens.Micro.Internal.TH (
  HasName (..),
  newNames,
  HasTypeVars (..),
  typeVars,
  setOf,
)
where

import Data.Monoid (Endo)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH (Con (ForallC, InfixC, RecC), Name, Type (AppT, ForallT, VarT))
import Language.Haskell.TH.Datatype.TyVarBndr (TyVarBndr_, traverseTVName)
import Language.Haskell.TH.Syntax (Con (NormalC), Q (Q), Quote (newName))
import Lens.Micro (Getting, (^.), (^..))
import Lens.Micro.Type (Lens', Traversal')

-- | Has a 'Name'
class HasName t where
  -- | Extract (or modify) the 'Name' of something
  name :: Lens' t Name

instance HasName (TyVarBndr_ flag) where
  name = traverseTVName

instance HasName Name where
  name = id

{- | On @template-haskell-2.11.0.0@ or later, if a 'GadtC' or 'RecGadtC' has
multiple 'Name's, the leftmost 'Name' will be chosen.
-}
instance HasName Con where
  name f (NormalC n tys) = (`NormalC` tys) <$> f n
  name f (RecC n tys) = (`RecC` tys) <$> f n
  name f (InfixC l n r) = (\n' -> InfixC l n' r) <$> f n
  name f (ForallC bds ctx con) = ForallC bds ctx <$> name f con

-- | Generate many new names from a given base name.
newNames ::
  -- | base name
  String ->
  -- | count
  Int ->
  Q [Name]
newNames base n = sequence [newName (base <> show i) | i <- [1 .. n]]

-- | Provides for the extraction of free type variables, and alpha renaming.
class HasTypeVars t where
  -- When performing substitution into this traversal you're not allowed
  -- to substitute in a name that is bound internally or you'll violate
  -- the 'Traversal' laws, when in doubt generate your names with 'newName'.
  typeVarsEx :: Set Name -> Traversal' t Name

instance HasTypeVars (TyVarBndr_ flag) where
  typeVarsEx s f b
    | Set.member (b ^. name) s = pure b
    | otherwise = name f b

instance HasTypeVars Name where
  typeVarsEx s f n
    | Set.member n s = pure n
    | otherwise = f n

instance HasTypeVars Type where
  typeVarsEx s f (VarT n) = VarT <$> typeVarsEx s f n
  typeVarsEx s f (AppT l r) = AppT <$> typeVarsEx s f l <*> typeVarsEx s f r
  typeVarsEx s f (ForallT bs ctx ty) = ForallT bs <$> typeVarsEx s' f ctx <*> typeVarsEx s' f ty
   where
    s' = s `Set.union` setOf typeVars bs

-- Traverse /free/ type variables
typeVars :: (HasTypeVars t) => Traversal' t Name
typeVars = typeVarsEx mempty

-----------------------------------------------------------------------------
-- Lens functions which would've been in LensMicro if it wasn't "micro"
-----------------------------------------------------------------------------
setOf :: (Ord a) => Getting (Endo [a]) s a -> s -> Set a
setOf l s = Set.fromList (s ^.. l)
