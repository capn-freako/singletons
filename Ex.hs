#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver nightly-2018-09-29 --package singletons

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns   #-}

module Ex where

import           Data.Kind
-- import           Data.Semigroup
import           Data.Singletons
import           Data.Singletons.Prelude hiding (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, sFoldr, SNil, SCons, Or, And, Id, Not, AndSym0, AndSym1, sAnd)
import           Data.Singletons.Prelude.Ord
import           Data.Singletons.TH hiding (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, sFoldr, Fold, sFold)
-- import           Data.Singletons.TypeLits
import           Data.Void

-- From the "Singletons to make things nicer" section of Lesson 4.
-- Replaces the more explicit definitions of: `DoorState`, `mergeState`, and
--`mergeStateList` (as well as all their singleton friends) found further down
-- (and, now, commented out).
$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)

  mergeState :: DoorState -> DoorState -> DoorState
  mergeState = max

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)

  mergeStateList :: [DoorState] -> DoorState
  mergeStateList = foldr mergeState Opened
  |])

-- $(singletons [d|
--   data DoorState = Opened | Closed | Locked
--     deriving (Show, Eq, Ord)
--   |])

data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }
  deriving (Show)

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

doorMat :: Door s -> String
doorMat (UnsafeMkDoor m) = m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

-- Ex. 1.1
unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m) =
  if n `mod` 2 /= 0
    then Just (UnsafeMkDoor m)
    else Nothing

-- Ex 1.2
-- This doesn't work!
--
-- openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
-- openAnyDoor n d = case sing of
--   SOpened -> Just d
--   SClosed -> Just $ openDoor d
--   SLocked -> openDoor <$> unlockDoor n d
--
-- It yields:
--
-- /Users/a594349/Documents/Projects/HaskellMisc/singletons/src/ex1_1.hs:36:19: error:
--     • Could not deduce (SingI a0) arising from a use of ‘sing’
--       from the context: SingI s
--         bound by the type signature for:
--                    openAnyDoor :: forall (s :: DoorState).
--                                   SingI s =>
--                                   Int -> Door s -> Maybe (Door 'Opened)
--         at /Users/a594349/Documents/Projects/HaskellMisc/singletons/src/ex1_1.hs:35:1-63
--       The type variable ‘a0’ is ambiguous
--       These potential instances exist:
--         instance SingI 'Closed
--           -- Defined at /Users/a594349/Documents/Projects/HaskellMisc/singletons/src/ex1_1.hs:19:1
--         instance SingI 'Locked
--           -- Defined at /Users/a594349/Documents/Projects/HaskellMisc/singletons/src/ex1_1.hs:19:1
--         instance SingI 'Opened
--           -- Defined at /Users/a594349/Documents/Projects/HaskellMisc/singletons/src/ex1_1.hs:19:1
--     • In the expression: sing
--       In the expression:
--         case sing of
--           SOpened -> Just d
--           SClosed -> Just $ openDoor d
--           SLocked -> openDoor <$> unlockDoor n d
--       In an equation for ‘openAnyDoor’:
--           openAnyDoor n d
--             = case sing of
--                 SOpened -> Just d
--                 SClosed -> Just $ openDoor d
--                 SLocked -> openDoor <$> unlockDoor n d
--    |
-- 36 | openAnyDoor n d = case sing of
--    |                   ^^^^^^^^^^^^...
--
-- This (Justin's solution) works. Why?
openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ = \case
      SOpened -> Just
      SClosed -> Just . openDoor
      SLocked -> fmap openDoor . unlockDoor n

-- DoorState isomorphism
toDS :: SomeSing DoorState -> DoorState
toDS (SomeSing s) = case s of
  SOpened -> Opened
  SClosed -> Closed
  SLocked -> Locked

toSS :: DoorState -> SomeSing DoorState
toSS = \case
  Opened -> SomeSing SOpened
  Closed -> SomeSing SClosed
  Locked -> SomeSing SLocked
  
-- Ex. 2.1
data OldSomeDoor :: Type where
  OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

showSomeDoor :: SomeDoor -> String
showSomeDoor (MkSomeDoor s d) = "Door: " ++ doorMat d ++ ", " ++ ds
 where
  ds = case s of
    SOpened -> "Opened"
    SClosed -> "Closed"
    SLocked -> "Locked"
    -- _       -> "Unknown"
    
fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds = case toSing ds of
    SomeSing s -> fromDoor s . mkDoor s

withDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor ds m f = withSomeSing ds $ \s -> f s (mkDoor s m)

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor s (UnsafeMkDoor m)) =
  OldMkSomeDoor (fromSing s) m
  
fromOld :: OldSomeDoor -> SomeDoor    
fromOld (OldMkSomeDoor ds m) = mkSomeDoor ds m

-- Ex. 2.2
unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor n d = withSomeSing ds $ \s ->
  mkSomeDoor (fromSing s) m
 where
  m  = doorMat d
  ds = case unlockDoor n d of
         Just _  -> Closed
         Nothing -> Locked

-- Ex. 2.3
-- openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n sd@(MkSomeDoor Sing d) =
  case openAnyDoor n d of
    Just d' -> fromDoor_ d'
    Nothing -> sd

-- Ex. 2.4
data List a = Nil | Cons a (List a)

data instance Sing (x :: List k) where
    SNil  :: Sing 'Nil
    SCons :: Sing x -> Sing xs -> Sing ('Cons x xs)

instance SingKind k => SingKind (List k) where
    -- type Demote (List k) = ???
    type Demote (List k) = List (Demote k)
      
    fromSing :: Sing (xs :: List k) -> List (Demote k)
    -- fromSing = ???
    fromSing = \case
      SNil       -> Nil
      SCons x xs -> fromSing x `Cons` fromSing xs
      
    toSing :: List (Demote k) -> SomeSing (List k)
    -- toSing = ???
    toSing = \case
      Nil       -> SomeSing SNil
      Cons x xs -> withSomeSing x $ \x' ->
        withSomeSing xs $ \xs' ->
          SomeSing $ SCons x' xs'

data Knockable :: DoorState -> Type where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

class Proved p a where
    auto :: p a

instance Proved Knockable 'Closed where
    auto = KnockClosed

instance Proved Knockable 'Locked where
    auto = KnockLocked

-- data Decision a = Proved a                  -- ^ a value of a exists
--                 | Disproved (Refuted a)     -- ^ a value of a cannot exist

-- | The data type with no values
-- data Void

-- | 'a' cannot exist.  Commonly also called `Not`
-- type Refuted a = a -> Void

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
  SOpened -> Disproved $ \case {}
  SClosed -> Proved KnockClosed
  SLocked -> Proved KnockLocked

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])

-- type family StatePass (s :: DoorState) :: Pass where
--     StatePass 'Opened = 'Allow
--     StatePass 'Closed = 'Obstruct
--     StatePass 'Locked = 'Obstruct
--
-- The below yields:
-- - the function, `statePass`, of the indicated type.
-- - the type family, above, using the same name.
-- - the function, `sStatePass :: Sing DoorState -> Sing (StatePass DoorState)`.
$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])
  
knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

-- Ex. 3.1
--
-- Interpretation of `SDoorState` as a predicate?
-- (SDoorState = Sing (s :: DoorState))
--
-- A singular value of type `SDoorState` serves as a run-time proxy for the existentially
-- quantified (and, therefore, hidden) type of a `Door` argument received by a function.
--
-- What “traditional” (that is, a -> Bool) predicate does it correspond to?
--
-- Well, since `SDoorState` is tertiary, there could be several.
-- The one we've been most concerend with is whether or not a particular door is locked.
-- So:
isLocked :: DoorState -> Bool
isLocked Opened = False
isLocked Closed = False
isLocked Locked = True
-- What is the type of its *decision function*? Can you implement it?
data Lockable :: DoorState -> Type where
  LockClosed :: Lockable 'Closed

isLockable :: Sing s -> Decision (Lockable s)
isLockable = \case
  SClosed -> Proved LockClosed
  SOpened -> Disproved $ \case {}
  SLocked -> Disproved $ \case {}
-- Hm, apparently I didn't understand the intent of the exercise.
-- Here is Justin's solution:
--
-- | 1. The predicate `SDoorState` (and really, the predicate for 
-- `Sing` for any specific kind instance) is essentially `const True`, the
-- "always-true" predicate.
--
decideDoorState :: Sing s -> Decision (SDoorState s)
decideDoorState = Proved

-- Ex. 3.2
refuteRefuteKnockable
    :: forall s. SingI s
    => Refuted (Refuted (Knockable s))  -- :: (Knockable s -> Void) -> Void
    -> Knockable s
refuteRefuteKnockable f = case isKnockable sing of
  Proved    t -> t
  Disproved g -> absurd $ f g

-- Ex. 3.3
-- a)
data AndP :: (k -> Type) -> (k -> Type) -> (k -> Type) where
  AndP :: p a -> q a -> AndP p q a
-- b)  
data Or :: (k -> Type) -> (k -> Type) -> (k -> Type) where
  OrLeft  :: p a -> Or p q a
  OrRight :: q a -> Or p q a

-- Do `AndP` and `Or` look similar to any types you might have encountered in the past?
-- Maybe, perhaps, similiar to types that are a part of basic beginner Haskell concepts?
--
-- Yes, `AndP` looks like a `Pair` (or, product) and `Or` looks like an `Either` (or, sum).

-- c)
decideAnd
    :: (forall x. Sing x -> Decision (p x))
    -> (forall x. Sing x -> Decision (q x))
    -> Sing a
    -> Decision (AndP p q a)
decideAnd p1 p2 x =
  case (p1 x, p2 x) of
    (Proved y, Proved z) -> Proved $ AndP y z
    (Disproved f, _)     -> Disproved $ \ (AndP y _) -> f y
    (_, Disproved g)     -> Disproved $ \ (AndP _ z) -> g z
    
decideOr
    :: (forall x. Sing x -> Decision (p x))
    -> (forall x. Sing x -> Decision (q x))
    -> Sing a
    -> Decision (Or p q a)
decideOr p1 p2 x =
  case (p1 x, p2 x) of
    (Disproved f, Disproved g) -> Disproved $ \case
      OrLeft  y -> f y
      OrRight z -> g z
    (Proved y, _)              -> Proved $ OrLeft  y
    (_, Proved z)              -> Proved $ OrRight z    

-- d)
-- instance SDecide DoorState where
--   SOpened %~ SOpened = Proved Refl
--   SOpened %~ SClosed = Disproved $ \case {}
--   SOpened %~ SLocked = Disproved $ \case {}  
--   SClosed %~ SClosed = Proved Refl
--   SClosed %~ SOpened = Disproved $ \case {}
--   SClosed %~ SLocked = Disproved $ \case {}    
--   SLocked %~ SLocked = Proved Refl  
--   SLocked %~ SOpened = Disproved $ \case {}
--   SLocked %~ SClosed = Disproved $ \case {}  
  
-- knockableNotOpened
--     :: forall s. SingI s
--     => Refuted (And Knockable ((:~:) 'Opened) s)
-- knockableNotOpened =
--   case decideAnd isKnockable (SOpened %~) sing of
--     Disproved f           -> f
--     Proved    (And pa qa) -> case pa of
--       KnockClosed -> case qa of {}
--       KnockLocked -> case qa of {}

-- Justin's solution is much simpler:
knockableNotOpened
    :: forall s. SingI s
    => Refuted (AndP Knockable ((:~:) 'Opened) s)
knockableNotOpened (AndP k o) = case k of
    KnockClosed -> case o of {} -- no constructor of type ('Opened :~: 'Closed)
    KnockLocked -> case o of {} -- no constructor of type ('Opened :~: 'Locked)

knockableOrOpened
    :: forall s. SingI s
    => Or Knockable ((:~:) 'Opened) s
knockableOrOpened = case sing @s of
  SOpened -> OrRight $ Refl @'Opened
  SClosed -> OrLeft KnockClosed
  SLocked -> OrLeft KnockLocked

-- Ex. 3.4
knockedRefute
    :: forall s. SingI s
    => Knockable s
    -> Refuted (s :~: 'Opened)
knockedRefute = \case
  KnockClosed -> \case {}
  KnockLocked -> \case {}  

refuteKnocked
    :: forall s. SingI s
    => Refuted (s :~: 'Opened)
    -> Knockable s
refuteKnocked f =  -- f :: (s :~: 'Opened) -> Void
  case sing @s of
    SOpened -> absurd . f $ Refl
    SClosed -> KnockClosed
    SLocked -> KnockLocked
    
-- Ex. 3.5
knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorRefl
    :: SomeDoor
    -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =  -- s :: Sing DoorState
  case sStatePass s %~ SObstruct of   -- sStatePass :: Sing DoorState -> Sing (StatePass DoorState)
    Proved    x -> knockRefl x d
    Disproved _ -> putStrLn "No knocking allowed!"

-- Ex. 3.6
$(singletons [d|
  invertPass :: Pass -> Pass
  invertPass Obstruct = Allow
  invertPass Allow    = Obstruct
  |])

knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorInv
    :: SomeDoor
    -> IO ()
knockSomeDoorInv (MkSomeDoor s d) =
-- Both work.
  -- Mine:
  -- case s of
  --   SOpened -> putStrLn "No knocking allowed!"
  --   SClosed -> knockInv d
  --   SLocked -> knockInv d
  -- Jason's:
  case sInvertPass (sStatePass s) of
    SObstruct -> putStrLn "No knocking allowed!"
    SAllow    -> knockInv d

-- Ex. 3.7
$(singletons [d|
  class Cycle a where
    next :: a -> a
    prev :: a -> a
  |])

instance Cycle DoorState where
    next Opened = Closed
    next Closed = Locked
    next Locked = Opened

    prev Opened = Locked
    prev Closed = Opened
    prev Locked = Closed

instance PCycle DoorState where
    type Next 'Opened = 'Closed
    type Next 'Closed = 'Locked
    type Next 'Locked = 'Opened

    type Prev 'Opened = 'Locked
    type Prev 'Closed = 'Opened
    type Prev 'Locked = 'Closed
    
instance SCycle DoorState where
  sNext SOpened = SClosed
  sNext SClosed = SLocked
  sNext SLocked = SOpened

  sPrev SOpened = SLocked
  sPrev SClosed = SOpened
  sPrev SLocked = SClosed
  
-- Lesson #4
data Hallway :: [DoorState] -> Type where
    HEnd  :: Hallway '[]        -- ^ end of the hallway, a stretch with no
                                --   doors
    (:<#) :: Door s
          -> Hallway ss
          -> Hallway (s ': ss)  -- ^ A door connected to a hallway is a new
                                --   hallway, and we track the door's state
                                --   in the list of hallway door states
infixr 5 :<#

-- See new consolidated definition near top of code.
-- $(singletons [d|
--   mergeState :: DoorState -> DoorState -> DoorState
--   mergeState = max
--   |])

mergeDoor
    :: Door s
    -> Door t
    -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (MkSomeDoor s d) (MkSomeDoor t e) =
    MkSomeDoor (sMergeState s t) (mergeDoor d e)
    
-- See new consolidated definition near top of code.
-- $(singletons [d|
--   mergeStateList :: [DoorState] -> DoorState
--   mergeStateList []     = Opened               -- ^ the identity of mergeState
--   mergeStateList (s:ss) = s `mergeState` mergeStateList ss
--   -- mergeStateList = foldr mergeState Opened
--   |])

-- data TyFun a b
-- type a ~> b = TyFun a b -> Type

-- infixr 0 ~>

data Id :: a ~> a

type instance Apply Id x = x

data Not :: Bool ~> Bool
type instance Apply Not 'False = 'True
type instance Apply Not 'True  = 'False

$(singletons [d|
  and :: Bool -> (Bool -> Bool)
  and False _ = False
  and True  x = x
  |])

-- The following definitions appear to be in Data.Singletons.TH.
--
-- data TyCon1
--         :: (j -> k)     -- ^ take a type constructor
--         -> (j ~> k)     -- ^ return a defunctionalization symbol
-- -- alternatively
-- -- data TyCon1 (t :: j -> k) :: j ~> k
--
-- type instance Apply (TyCon1 t) a = t a
--
-- type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
--     Foldr f z '[]       = z
--     Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs

-- And these are coming from my singletons template for `mergeState` above.
--
-- data MergeStateSym0 :: DoorState ~> DoorState ~> DoorState
-- type instance Apply MergeStateSym0 s = MergeStateSym1 s
--
-- data MergeStateSym1 :: DoorState G-> DoorState ~> DoorState
-- type instance Apply (MergeStateSym1 s) t = MergeState s t
--
-- type MergeStateSym2 s t = MergeState s t

-- type MergeStateList ss = Foldr MergeStateSym0 'Opened ss

-- The following causes a long compiler error, when `MergeStateList` is defined by the line above,
-- as opposed to using the singletons TH for `mergeStateList`, far above.
--
collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = mkDoor SOpened "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds

$(singletons [d|
  instance Semigroup DoorState where
      (<>) = mergeState
  instance Monoid DoorState where
      mempty  = Opened
      mappend = (<>)
  |])

$(singletons [d|
  fold :: Monoid b => [b] -> b
  fold []     = mempty
  fold (x:xs) = x <> fold xs
  |])

