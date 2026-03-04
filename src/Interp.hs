module Interp (interp, normalize, reduce, reduceGuard) where

import qualified Data.Set as S
import Language
import qualified Data.Map.Strict as M

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


-- Interpreter with environment
type ProcEnv = M.Map PName ([O_Name], O_Process)

interp :: ProcEnv -> O_Process -> S.Set O_Process
interp env p =
  let p' = normalizeO p
      next = reduceEnvO env p'
  in if S.null next then S.singleton p' else S.unions (S.map (interp env) next)

-- Reduction with environment for O_Process
reduceEnvO :: ProcEnv -> O_Process -> S.Set O_Process
reduceEnvO env Done = S.empty
reduceEnvO env (Parallel p1 p2) = S.map (`Parallel` p2) (reduceEnvO env p1) `S.union` S.map (Parallel p1) (reduceEnvO env p2)
reduceEnvO env (New u iname p) = S.map (New u iname) (reduceEnvO env p)
reduceEnvO env (Output u tag vs) = S.empty
reduceEnvO env (Input u pat g) = reduceGuardEnvO env u g
reduceEnvO env (Call pname vs) =
  case M.lookup pname env of
    Just (params, body) ->
      let subst = M.fromList (zip (map fst params) vs) -- subst :: M.Map Name O_Name
      in S.singleton (substProcessO subst body)
    Nothing -> S.empty

-- Guard reduction for O_Process
reduceGuardEnvO :: ProcEnv -> O_Name -> Guard O_Name -> S.Set O_Process
reduceGuardEnvO env u (Fail _) = S.empty
reduceGuardEnvO env u (Free p) = S.singleton p
reduceGuardEnvO env u (Select tag xs p) = S.singleton p
reduceGuardEnvO env u (Choice g1 g2) = reduceGuardEnvO env u g1 `S.union` reduceGuardEnvO env u g2

substProcessO :: M.Map Name O_Name -> O_Process -> O_Process
substProcessO subst Done = Done
substProcessO subst (Output u tag vs) = Output (substNameO subst u) tag (map (substNameO subst) vs)
substProcessO subst (Input u pat g) = Input (substNameO subst u) pat (substGuardO subst g)
substProcessO subst (Parallel p1 p2) = Parallel (substProcessO subst p1) (substProcessO subst p2)
substProcessO subst (New u iname p) = New (substNameO subst u) iname (substProcessO subst p)
substProcessO subst (Call pname vs) = Call pname (map (substNameO subst) vs)

substGuardO :: M.Map Name O_Name -> Guard O_Name -> Guard O_Name
substGuardO subst (Fail msg) = Fail msg
substGuardO subst (Free p) = Free (substProcessO subst p)
substGuardO subst (Select tag xs p) = Select tag (map (substNameO subst) xs) (substProcessO subst p)
substGuardO subst (Choice g1 g2) = Choice (substGuardO subst g1) (substGuardO subst g2)

substNameO :: M.Map Name O_Name -> O_Name -> O_Name
substNameO subst (n, t) = M.findWithDefault (n, t) n subst
-- Structural congruence normalization for O_Process
normalizeO :: O_Process -> O_Process
normalizeO (Parallel Done p) = normalizeO p
normalizeO (Parallel p Done) = normalizeO p
normalizeO (Parallel (Parallel p1 p2) p3) = normalizeO (Parallel p1 (Parallel p2 p3))
normalizeO (Parallel p1 p2) = Parallel (normalizeO p1) (normalizeO p2)
normalizeO (New u iname (New v iname' p)) = normalizeO (New v iname' (New u iname p))
normalizeO (New u iname p) = New u iname (normalizeO p)
normalizeO p = p
