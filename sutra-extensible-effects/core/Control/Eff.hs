{-# LANGUAGE ExplicitNamespaces #-}

module Control.Eff (
  -- * Effect type
  Internal.run,
  Internal.Eff,

  -- * Lift IO computations
  Internal.lift,
  Internal.runLift,
  Internal.catchDynE,
  Internal.HandlerDynE (..),
  Internal.catchesDynE,
  Internal.Lift (..),
  Internal.Lifted,
  Internal.LiftedBase,

  -- * Effect list
  OpenUnion.Member,
  OpenUnion.SetMember,
  type (<::),
)
where

import Control.Eff.Internal as Internal
import Data.OpenUnion as OpenUnion
