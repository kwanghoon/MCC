module Interp (interp, normalize, reduce, reduceGuard) where

import qualified Data.Set as S
import Language

-- Structural congruence normalization
normalize :: Ord u => Process u -> Process u
normalize (Parallel Done p) = normalize p
normalize (Parallel p Done) = normalize p
normalize (Parallel (Parallel p1 p2) p3) = normalize (Parallel p1 (Parallel p2 p3))
normalize (Parallel p1 p2) = Parallel (normalize p1) (normalize p2)
normalize (New u iname (New v iname' p)) = normalize (New v iname' (New u iname p))
normalize (New u iname p) = New u iname (normalize p)
normalize p = p

-- Reduction rules (one-step)
reduce :: (Ord u) => Process u -> S.Set (Process u)
reduce Done = S.empty
reduce (Parallel p1 p2) = S.map (`Parallel` p2) (reduce p1) `S.union` S.map (Parallel p1) (reduce p2)
reduce (New u iname p) = S.map (New u iname) (reduce p)
reduce (Output u tag vs) = S.empty -- No reduction unless matched with Input
reduce (Input u pat g) = reduceGuard u g
reduce (Call pname vs) = S.empty -- Needs process definition environment

-- Guard reduction (simplified)
reduceGuard :: Ord u => u -> Guard u -> S.Set (Process u)
reduceGuard u (Fail _) = S.empty
reduceGuard u (Free p) = S.singleton p
reduceGuard u (Select tag xs p) = S.singleton p
reduceGuard u (Choice g1 g2) = reduceGuard u g1 `S.union` reduceGuard u g2

-- Interpreter: computes reachable process set
interp :: Ord u => Process u -> S.Set (Process u)
interp p =
  let p' = normalize p
      next = reduce p'
  in if S.null next then S.singleton p' else S.unions (S.map interp next)
