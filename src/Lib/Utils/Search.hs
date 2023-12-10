{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Utils.Search
  ( bfs,
    dfs,
  )
where

import qualified Data.Map as M

-- TODO should this even be a `Map` for `start`?
bfs :: forall k v. (Ord k) => (k -> v -> M.Map k v) -> M.Map k v -> M.Map k v
bfs next start = go start mempty
  where
    go :: M.Map k v -> M.Map k v -> M.Map k v
    go frontier seen
      | M.null frontier = seen
      | otherwise = go frontier' seen'
      where
        frontier' = (`M.difference` seen) . mconcat . fmap (uncurry next) . M.toList $ frontier
        seen' = M.union seen frontier

dfs :: forall k v. (Ord k) => (k -> v -> [(k, v)]) -> k -> v -> M.Map k v
-- TODO don't really love needing a starting `val`
dfs next start val = go (M.singleton start val) (next start val)
  where
    go :: M.Map k v -> [(k, v)] -> M.Map k v
    go seen [] = seen
    go seen (q : queue)
      -- TODO probably not the most efficient to check member here and then insert later?
      | fst q `M.member` seen = go seen queue
      | otherwise = go seen' (queue' <> queue)
      where
        (k, v) = q
        seen' = M.insert k v seen
        queue' = filter (not . (`M.member` seen') . fst) $ next k v
