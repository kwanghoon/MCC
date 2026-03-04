-- | Simulation of the mailbox calculus operational semantics.
--
-- Instead of applying structural congruence rules (which easily diverge),
-- we use a /canonical flat representation/:
--
--   * Parallel composition is a multiset (list) of 'Atom's — commutativity,
--     associativity, and the @done@ unit law hold by construction.
--
--   * All ν-binders are hoisted to the top via scope extrusion — the
--     congruence @(νa)P | Q ≡ (νa)(P | Q)@ is built into the representation.
--
--   * Guards are flattened to a list of 'Action's, discarding @Fail@ branches
--     (since @fail a + G ≡ G@).
--
-- Reduction rules are applied directly on this canonical form:
--
--   * __R-READ__:  @a!m[c̄] | a?π.(m(x̄).P + G) → P{c̄\/x̄}@
--   * __R-FREE__:  @(νa)(free a.P + G) → P@  when no messages pending on @a@
--   * __R-DEF__:   @X[c̄] → P{c̄\/x̄}@  (need-driven: only when no R-READ\/R-FREE)
--
-- This ensures termination of the normalisation phase while faithfully
-- simulating every possible reduction step.

module Simulation
  ( -- * Flat representation types
    Atom(..)
  , Action(..)
  , Config(..)
  , StepLabel(..)
    -- * Conversion from Language AST
  , fromProcess
  , initConfig
    -- * Reduction
  , step
  , tryRead
  , tryFree
  , tryDef
  , isFinal
    -- * Simulation
  , simulate
  , simulateIO
    -- * Pretty printing
  , showConfig
  , showAtom
  , showAction
  ) where

import Language
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate)

------------------------------------------------------------------------
-- Flat representation types
------------------------------------------------------------------------

-- | An atomic process (one non‐parallel component in the soup).
data Atom
  = AOutput O_Name Tag [O_Name]       -- ^ @a!m(c₁, …, cₖ)@
  | AInput  O_Name Pattern [Action]   -- ^ @a?π.\{act₁ & … & actₙ\}@
  | ACall   PName [O_Name]            -- ^ @X(c₁, …, cₖ)@
  deriving (Eq, Ord)

-- | A branch in a flattened guard.
data Action
  = AFree   O_Process                  -- ^ @free ▸ P@
  | ASelect Tag [O_Name] O_Process    -- ^ @m(x₁, …, xₖ) ▸ P@
  deriving (Eq, Ord)

-- | A simulation configuration.
--
-- Represents @(νa₁:I₁)…(νaₙ:Iₙ)(P₁ | … | Pₘ)@ where each @Pᵢ@ is
-- an 'Atom'.
data Config = Config
  { cfgBinders :: [(O_Name, IName)]   -- ^ ν-restricted names with interfaces
  , cfgAtoms   :: [Atom]              -- ^ parallel soup of atomic processes
  , cfgFresh   :: Int                 -- ^ counter for fresh name generation
  } deriving (Eq, Ord)

-- | Label describing which reduction rule was applied.
data StepLabel
  = LRead O_Name Tag   -- ^ R-READ: communication on mailbox with given tag
  | LFree O_Name       -- ^ R-FREE: mailbox freed
  | LDef  PName        -- ^ R-DEF: process definition unfolded
  deriving (Eq, Ord)

instance Show StepLabel where
  show (LRead (n, _) tag) = "R-READ " ++ show n ++ "!" ++ show tag
  show (LFree (n, _))     = "R-FREE " ++ show n
  show (LDef pname)       = "R-DEF  " ++ show pname

------------------------------------------------------------------------
-- Conversion from Language.O_Process to Config
------------------------------------------------------------------------

-- | Convert an 'O_Process' to a flat 'Config'.
--
-- All @New@ binders are α-renamed to fresh names, then hoisted to the
-- top.  Parallel composition is decomposed into a list of 'Atom's.
fromProcess :: O_Process -> Config
fromProcess p =
  let (ctr, p') = alphaRenameNew 0 p
      (bs, as)  = flatten p'
  in Config bs as ctr

-- | Build the initial 'Config' by looking up the @Main@ process.
initConfig :: ProcessDefinitions -> Maybe Config
initConfig env = do
  let mainName = Identifier Somewhere "Main" :: PName
  (_, body) <- M.lookup mainName env
  return (fromProcess body)

-- | Decompose an 'O_Process' into (ν-binders, atoms).
--
-- 'Done' disappears, 'Parallel' is flattened, 'New' is hoisted.
flatten :: O_Process -> ([(O_Name, IName)], [Atom])
flatten Done              = ([], [])
flatten (Output u tag vs) = ([], [AOutput u tag vs])
flatten (Input u pat g)   = ([], [AInput u pat (flattenGuard g)])
flatten (Parallel p1 p2)  =
  let (bs1, as1) = flatten p1
      (bs2, as2) = flatten p2
  in (bs1 ++ bs2, as1 ++ as2)
flatten (New u iname p) =
  let (bs, as) = flatten p
  in ((u, iname) : bs, as)
flatten (Call pname vs) = ([], [ACall pname vs])

-- | Flatten a guard into a list of 'Action's.
--
-- 'Fail' branches are discarded (since @fail a + G ≡ G@).
-- 'Choice' is decomposed into concatenation.
flattenGuard :: O_Guard -> [Action]
flattenGuard (Fail _)          = []
flattenGuard (Free p)          = [AFree p]
flattenGuard (Select tag xs p) = [ASelect tag xs p]
flattenGuard (Choice g1 g2)    = flattenGuard g1 ++ flattenGuard g2

------------------------------------------------------------------------
-- Alpha-renaming of ν-binders
------------------------------------------------------------------------

-- | Rename all @New@-bound names to globally fresh names.
--
-- This is essential to avoid variable capture when a recursive process
-- definition is unfolded multiple times.  Only @New@ binders are
-- renamed; @Select@ binders (which act like λ-bindings) are left alone
-- since they are substituted away upon communication.
alphaRenameNew :: Int -> O_Process -> (Int, O_Process)
alphaRenameNew ctr Done = (ctr, Done)
alphaRenameNew ctr (Output u tag vs) = (ctr, Output u tag vs)
alphaRenameNew ctr (Input u pat g) =
  let (ctr', g') = alphaRenameGuard ctr g
  in (ctr', Input u pat g')
alphaRenameNew ctr (Parallel p1 p2) =
  let (c1, p1') = alphaRenameNew ctr p1
      (c2, p2') = alphaRenameNew c1  p2
  in (c2, Parallel p1' p2')
alphaRenameNew ctr (New (u, t) iname p) =
  let freshN     = Identifier Somewhere (identifierText u ++ "#" ++ show ctr)
      s          = M.singleton u (freshN, t)
      p'         = substProcess s p
      (ctr', p'') = alphaRenameNew (ctr + 1) p'
  in (ctr', New (freshN, t) iname p'')
alphaRenameNew ctr (Call pname vs) = (ctr, Call pname vs)

alphaRenameGuard :: Int -> O_Guard -> (Int, O_Guard)
alphaRenameGuard ctr (Fail msg) = (ctr, Fail msg)
alphaRenameGuard ctr (Free p) =
  let (ctr', p') = alphaRenameNew ctr p
  in (ctr', Free p')
alphaRenameGuard ctr (Select tag xs p) =
  let (ctr', p') = alphaRenameNew ctr p
  in (ctr', Select tag xs p')
alphaRenameGuard ctr (Choice g1 g2) =
  let (c1, g1') = alphaRenameGuard ctr g1
      (c2, g2') = alphaRenameGuard c1  g2
  in (c2, Choice g1' g2')

------------------------------------------------------------------------
-- Substitution
------------------------------------------------------------------------

type Subst = M.Map Name O_Name

substName :: Subst -> O_Name -> O_Name
substName s (n, t) = M.findWithDefault (n, t) n s

substProcess :: Subst -> O_Process -> O_Process
substProcess _ Done = Done
substProcess s (Output u tag vs) =
  Output (substName s u) tag (map (substName s) vs)
substProcess s (Input u pat g) =
  Input (substName s u) pat (substGuard s g)
substProcess s (Parallel p1 p2) =
  Parallel (substProcess s p1) (substProcess s p2)
substProcess s (New u iname p) =
  -- Remove the binder from the substitution to respect shadowing
  let s' = M.delete (fst u) s
  in New u iname (substProcess s' p)
substProcess s (Call pname vs) =
  Call pname (map (substName s) vs)

substGuard :: Subst -> O_Guard -> O_Guard
substGuard _ (Fail msg) = Fail msg
substGuard s (Free p) = Free (substProcess s p)
substGuard s (Select tag xs p) =
  -- xs are binders: remove them from the substitution
  let s' = foldr (M.delete . fst) s xs
  in Select tag xs (substProcess s' p)
substGuard s (Choice g1 g2) =
  Choice (substGuard s g1) (substGuard s g2)

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

-- | All ways to pick one element from a list, returning (picked, rest).
picks :: [a] -> [(a, [a])]
picks []     = []
picks (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- picks xs]

-- | Compare 'O_Name's by their 'Name' component only (ignoring type).
nameEq :: O_Name -> O_Name -> Bool
nameEq (n1, _) (n2, _) = n1 == n2

-- | Check if a name is ν-bound in the given binder list.
isBound :: O_Name -> [(O_Name, IName)] -> Bool
isBound (n, _) = any (\((m, _), _) -> n == m)

-- | Remove all binders for a name from the binder list.
removeBinder :: O_Name -> [(O_Name, IName)] -> [(O_Name, IName)]
removeBinder (n, _) = filter (\((m, _), _) -> n /= m)

-- | Check if there are any pending output messages on a mailbox.
hasPendingMsg :: O_Name -> [Atom] -> Bool
hasPendingMsg (n, _) = any match
  where
    match (AOutput (m, _) _ _) = n == m
    match _                    = False

-- | Free names of an 'Atom' (all names it could reference).
atomFreeNames :: Atom -> S.Set Name
atomFreeNames (AOutput (n, _) _ vs) = S.fromList (n : map fst vs)
atomFreeNames (AInput  (n, _) _ acts) =
  S.insert n (S.unions (map actionFreeNames acts))
atomFreeNames (ACall _ vs) = S.fromList (map fst vs)

-- | Free names of an 'Action' (including deep inside continuations).
actionFreeNames :: Action -> S.Set Name
actionFreeNames (AFree body) = S.map fst (fnp body)
actionFreeNames (ASelect _ xs body) =
  S.map fst (fnp body) `S.difference` S.fromList (map fst xs)

-- | Check if any atom in the list references the given name.
anyReferences :: O_Name -> [Atom] -> Bool
anyReferences (n, _) = any (\a -> n `S.member` atomFreeNames a)

------------------------------------------------------------------------
-- Reduction rules
------------------------------------------------------------------------

-- | __R-READ__: @a!m[c̄] | a?π.(m(x̄).P + G) → P{c̄\/x̄}@
--
-- Find an output @a!m[c̄]@ and an input on the same mailbox @a@ whose
-- guard has a 'Select' branch matching tag @m@.  Consume both the
-- output and the entire input; produce the continuation with the
-- bound variables substituted.
tryRead :: Config -> [(StepLabel, Config)]
tryRead cfg =
  [ (LRead a tag, result)
  | (AOutput a tag cs,  rest1) <- picks (cfgAtoms cfg)
  , (AInput b _pat actions, rest2) <- picks rest1
  , nameEq a b
  , ASelect stag xs body <- actions
  , stag == tag
  , length xs == length cs
  , let -- α-rename New binders first to avoid variable capture
        (ctr', body')  = alphaRenameNew (cfgFresh cfg) body
        -- Then substitute the Select-bound variables
        subst          = M.fromList (zip (map fst xs) cs)
        body''         = substProcess subst body'
        (newBs, newAs) = flatten body''
        result = Config (cfgBinders cfg ++ newBs) (rest2 ++ newAs) ctr'
  ]

-- | __R-FREE__: @(νa)(free a.P + G) → P@
--
-- Applicable when mailbox @a@ is ν-restricted, has no pending
-- output messages in the soup, and no other atom references the
-- mailbox (ensuring no future messages can arrive).
tryFree :: Config -> [(StepLabel, Config)]
tryFree cfg =
  [ (LFree a, result)
  | (AInput a _pat actions, rest) <- picks (cfgAtoms cfg)
  , isBound a (cfgBinders cfg)
  , not (anyReferences a rest)
  , AFree body <- actions
  , let (ctr', body') = alphaRenameNew (cfgFresh cfg) body
        (newBs, newAs) = flatten body'
        result = Config (removeBinder a (cfgBinders cfg) ++ newBs)
                        (rest ++ newAs)
                        ctr'
  ]

-- | __R-DEF__: @X[c̄] → P{c̄\/x̄}@  (need-driven)
--
-- Unfold a process definition.  This rule is only applied when no
-- R-READ or R-FREE reduction is available (need-driven strategy),
-- preventing infinite unfolding of recursive definitions.
tryDef :: ProcessDefinitions -> Config -> [(StepLabel, Config)]
tryDef env cfg =
  [ (LDef pname, result)
  | (ACall pname vs, rest) <- picks (cfgAtoms cfg)
  , Just (params, body)    <- [M.lookup pname env]
  , let -- α-rename New binders first to avoid variable capture
        (ctr', body')  = alphaRenameNew (cfgFresh cfg) body
        -- Then substitute formal parameters with actual arguments
        subst          = M.fromList (zip (map fst params) vs)
        body''         = substProcess subst body'
        (newBs, newAs) = flatten body''
        result = Config (cfgBinders cfg ++ newBs) (rest ++ newAs) ctr'
  ]

------------------------------------------------------------------------
-- Step function with need-driven strategy
------------------------------------------------------------------------

-- | Compute all possible one-step reductions.
--
-- __Priority__: R-READ and R-FREE are tried first.  R-DEF (process
-- definition unfolding) is applied /only/ when no communication or
-- free reduction is possible.  This need-driven strategy prevents
-- runaway unfolding of recursive process definitions.
step :: ProcessDefinitions -> Config -> [(StepLabel, Config)]
step env cfg =
  let comms = tryRead cfg
      frees = tryFree cfg
  in if not (null comms) || not (null frees)
     then comms ++ frees
     else tryDef env cfg

-- | Check if no more reductions are possible.
isFinal :: ProcessDefinitions -> Config -> Bool
isFinal env cfg = null (step env cfg)

------------------------------------------------------------------------
-- Simulation drivers
------------------------------------------------------------------------

-- | Run simulation up to @maxSteps@, always choosing the first
-- available reduction at each step (deterministic exploration).
-- Returns the sequence of (label, resulting config) pairs.
simulate :: ProcessDefinitions -> Int -> Config -> [(StepLabel, Config)]
simulate _ 0 _ = []
simulate env n cfg =
  case step env cfg of
    []                -> []
    ((label, cfg'):_) -> (label, cfg') : simulate env (n - 1) cfg'

-- | Run simulation and print each step to stdout.
simulateIO :: ProcessDefinitions -> Int -> Config -> IO ()
simulateIO env maxSteps cfg0 = do
  putStrLn $ "  " ++ showConfig cfg0
  go 1 maxSteps cfg0
  where
    go _ 0 _ = putStrLn "(max steps reached)"
    go i n cfg =
      case step env cfg of
        [] -> putStrLn "(done — no more reductions)"
        ((label, cfg'):_) -> do
          putStrLn $ show i ++ ". " ++ show label
          putStrLn $ "  " ++ showConfig cfg'
          go (i + 1) (n - 1) cfg'

------------------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------------------

instance Show Atom where show = showAtom
instance Show Action where show = showAction
instance Show Config where show = showConfig

showConfig :: Config -> String
showConfig cfg =
  let bs = concatMap showBinder (cfgBinders cfg)
      as = case cfgAtoms cfg of
             [] -> "done"
             xs -> intercalate " | " (map showAtom xs)
  in bs ++ as

showBinder :: (O_Name, IName) -> String
showBinder ((n, _), iname) =
  "(ν" ++ show n ++ ":" ++ show iname ++ ")"

showAtom :: Atom -> String
showAtom (AOutput (n, _) tag vs) =
  show n ++ "!" ++ show tag ++ showNames vs
showAtom (AInput (n, _) _pat actions) =
  show n ++ "?{" ++ intercalate " & " (map showAction actions) ++ "}"
showAtom (ACall pname vs) =
  show pname ++ showNames vs

showNames :: [O_Name] -> String
showNames [] = ""
showNames xs = "(" ++ intercalate ", " [show n | (n, _) <- xs] ++ ")"

showAction :: Action -> String
showAction (AFree _)          = "free ▸ …"
showAction (ASelect tag xs _) = show tag ++ showNames xs ++ " ▸ …"
