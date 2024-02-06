{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.FTCQueue (
  FTCQueue,
  tsingleton,
  (|>),
  (><),
  ViewL,
  viewlMap,
  tviewl,
) where

{- | Non-empty tree. Deconstruction operations make it more and more
left-leaning.
-}
data FTCQueue m a b where
  Leaf :: (a -> m b) -> FTCQueue m a b
  Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b

-- Exported operations

{- | There is no @tempty@: use (@tsingleton return@), which works just the same.
The names are chosen for compatibility with FastTCQueue
-}
{-# INLINE tsingleton #-}
tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = Leaf

-- | snoc: clearly constant-time
{-# INLINE (|>) #-}
(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
t |> r = Node t (Leaf r)

infixl 9 |>

-- | append: clearly constant-time
(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(><) = Node

infixl 9 ><

-- | Left-edge deconstruction
data ViewL m a b where
  TOne :: (a -> m b) -> ViewL m a b
  (:|) :: (a -> m x) -> (FTCQueue m x b) -> ViewL m a b

-- | Process the Left-edge deconstruction
viewlMap ::
  ViewL m a b ->
  ((a -> m b) -> c) ->
  (forall x. (a -> m x) -> FTCQueue m x b -> c) ->
  c
viewlMap view tone cons = case view of
  TOne k -> tone k
  k :| t -> cons k t

{-# INLINE tviewl #-}
tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r) = TOne r
tviewl (Node t1 t2) = go t1 t2
 where
  go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
  go (Leaf r) tr = r :| tr
  go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)
