#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver nightly-2018-09-29 --package singletons

{-# LANGUAGE AllowAmbiguousTypes            #-}
{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE NoStarIsType                   #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeInType                     #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}
{-# OPTIONS_GHC -Wall                       #-}
-- {-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude hiding    (And, Or, sFoldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Foldr)
import           Data.Singletons.Sigma
import           Data.Singletons.TH hiding         (sFoldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Foldr, sFold, Fold)
import           Data.Singletons.TypeLits

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

$(singletons [d|
  mergeState :: DoorState -> DoorState -> DoorState
  mergeState = max
  |])

mergeDoor
    :: Door s
    -> Door t
    -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

type SomeDoor = Sigma DoorState (TyCon1 Door)

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    dsSing :&: mkDoor dsSing mat

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (s :&: d) (t :&: e) =
    sMergeState s t :&: mergeDoor d e

data Hallway :: [DoorState] -> Type where
    HEnd  :: Hallway '[]        -- ^ end of the hallway, a stretch with no
                                --   doors
    (:<#) :: Door s
          -> Hallway ss
          -> Hallway (s ': ss)  -- ^ A door connected to a hallway is a new
                                --   hallway, and we track the door's state
                                --   in the list of hallway door states
infixr 5 :<#

$(singletons [d|
  mergeStateList :: [DoorState] -> DoorState
  mergeStateList []     = Opened               -- ^ the identity of mergeState
  mergeStateList (s:ss) = s `mergeState` mergeStateList ss
  |])

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = mkDoor SOpened "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds

type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = sMergeStateList ss
                             :&: collapseHallway d

$(singletons [d|
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)
  |])

-- | COMMENT THIS OUT IF YOU WANT TO RUN ON SINGLETONS < 2.5 OR GHC 8.4
$(singletons [d|
  fold :: Monoid b => [b] -> b
  fold []     = mempty
  fold (x:xs) = x <> fold xs

  instance Semigroup DoorState where
      (<>) = mergeState
  instance Monoid DoorState where
      mempty  = Opened
      mappend = (<>)
  |])

collapseHallway'
    :: Hallway ss
    -> Door (Fold ss)
collapseHallway' HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway' (d :<# ds) = d `mergeDoor` collapseHallway' ds

collapseSomeHallway' :: SomeHallway -> SomeDoor
collapseSomeHallway' (ss :&: d) =
        sFold ss
    :&: collapseHallway' d

-- | END OF SINGLETONS-2.5 ONLY SECTON

collapseHallway''
    :: Hallway ss
    -> Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
collapseHallway'' HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway'' (d :<# ds) = d `mergeDoor` collapseHallway'' ds

collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) =
        sFoldr (singFun2 @MergeStateSym0 sMergeState) SOpened ss
     -- or
     -- sFoldr (sing @MergeStateSym0) SOpened ss
    :&: collapseHallway'' d

-- Ex. 4.1
data Knockable :: DoorState -> Type where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

mergedIsKnockable
    :: Knockable s
    -> Knockable t
    -> Knockable (MergeState s t)
mergedIsKnockable KnockClosed KnockClosed = KnockClosed
mergedIsKnockable KnockClosed KnockLocked = KnockLocked
mergedIsKnockable KnockLocked KnockClosed = KnockLocked
mergedIsKnockable KnockLocked KnockLocked = KnockLocked
  
-- Ex. 4.2
-- data Hallway :: [DoorState] -> Type where
--     HEnd  :: Hallway '[]        -- ^ end of the hallway, a stretch with no
--                                 --   doors
--     (:<#) :: Door s
--           -> Hallway ss
--           -> Hallway (s ': ss)  -- ^ A door connected to a hallway is a new
--                                 --   hallway, and we track the door's state
--                                 --   in the list of hallway door states
-- infixr 5 :<#
appendHallways
    :: Hallway ss
    -> Hallway ts
    -> Hallway (ss ++ ts)
appendHallways HEnd tss = tss
appendHallways (ds :<# hss) tss
  = ds :<# appendHallways hss tss

-- data Sigma k :: (k ~> Type) -> Type where
--     (:&:) :: Sing x -> (f @@ x) -> Sigma k f

-- type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

appendSomeHallways
    :: SomeHallway
    -> SomeHallway
    -> SomeHallway
appendSomeHallways (sx :&: fAtX) (sy :&: gAtY) =
  (sx `sMappend` sy) :&: appendHallways fAtX gAtY

-- Ex. 4.3
-- data KnockableDoor :: DoorState ~> (Knockable s, Door s)
data KnockableDoor :: DoorState ~> Type

type instance Apply KnockableDoor (s :: DoorState) = (Knockable s, Door s)

type SomeKnockableDoor = Sigma DoorState KnockableDoor

mkKnockableDoor
  :: forall s. SingI s
  => Knockable s -> String -> SomeKnockableDoor
mkKnockableDoor s mat =
  ss :&: (s, mkDoor ss mat)
 where
  ss = sing @s

-- Ex. 4.4
data IsHalfOf :: Nat -> Nat ~> Type
type instance Apply (IsHalfOf n) m = n :~: (m * 2)

type IsEven n = Sigma Nat (IsHalfOf n)

tenIsEven :: IsEven 10
tenIsEven = SNat @5 :&: Refl @10
    -- Refl is the constructor of type n :~: (m * 2)
    -- here, we use it as Refl @10 :: 10 :~: 10

-- won't compile
-- sevenIsEven :: IsEven 7
-- sevenIsEven = SNat @4 :&: Refl
    -- won't compile, because we need something of type `(4 * 2) :~: 7`,
    -- but Refl must have type `a :~: a`; `8 :~: 7` is not constructable
    -- using `Refl`.  Neither `Refl @8` nor `Refl @7` will work.

data HasRem :: Nat ~> Type

type instance Apply HasRem n = 1 :~: (Rem n 2)

type IsOdd n = Sigma Nat HasRem

sevenIsOdd :: IsOdd 7
sevenIsOdd = SNat @7 :&: Refl @1

-- Ex. 4.5
-- Directly implement a type-level Map, with kind (j ~> k) -> [j] -> [k], in terms of Foldr:
-- type Map f xs = Foldr ???? ???? xs
--
-- type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
--     Foldr f z '[]       = z
--     Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs
--
-- type TyCon2 = (TyCon :: (k1 -> k2 -> k3) -> k1 ~> (k2 ~> k3))
--
-- (:.) :: (b ~> c) -> (a ~> b) -> a -> c
-- (:)  :: a -> [a] -> [a]
-- TyCon2 (:) :: a ~> ([a] ~> [a])
-- TyCon2 (:) :. (f :: j ~> k) :: j -> ([k] ~> [k])
-- TyCon3 (:.) :: (b ~> k4) ~> ((k3 ~> b) ~> (k3 ~> k4))
-- TyCon3 (:.) @@ TyCon2 (:) :: (k3 ~> b) ~> (k3 ~> ([b] ~> [b]))
-- type Map (f :: j ~> k) (xs :: [j]) = Foldr (TyCon1 (TyCon2 (:) :. f)) '[] xs
-- type Map (f :: j ~> k) (xs :: [j]) =
--   Foldr ((TyCon3 (:.)) @@ TyCon2 (:) @@ f) '[] xs
--
-- Justin's solution:
type Map f xs = Foldr (TyCon2 (:) .@#@$$$ f) '[] xs

-- From his *Singletons to make things nicer* section:
--
-- For operator names like ++, the naming convention is to have ++@#@$ be the completely unapplied defunctionalization symbol, ++@#@$$ be the type constructor that expects one argument before returning a defunctionalization symbol, ++@#@$$$ be the type constructor that takes two arguments before returning a defunctionalization symbol, etc.

-- Ex. 4.6
-- data Hallway :: [DoorState] -> Type where
--     HEnd  :: Hallway '[]        -- ^ end of the hallway, a stretch with no
--                                 --   doors
--     (:<#) :: Door s
--           -> Hallway ss
--           -> Hallway (s ': ss)  -- ^ A door connected to a hallway is a new
--                                 --   hallway, and we track the door's state
--                                 --   in the list of hallway door states
-- infixr 5 :<#
--
-- type SomeDoor    = Sigma DoorState   (TyCon1 Door)
-- type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)
--
-- type TyCon1 = (TyCon :: (k1 -> k2) -> k1 ~> k2)
--
-- data Sigma k :: (k ~> Type) -> Type where
--     (:&:) :: Sing x -> (f @@ x) -> Sigma k f
--
-- SCons :: Sing n1 -> Sing n2 -> Sing (n1 : n2)
mkSomeHallway :: [SomeDoor] -> SomeHallway
mkSomeHallway [] = SNil :&: HEnd
mkSomeHallway (sd : sds) = case sd of
  (s :&: d) -> case mkSomeHallway sds of
    (ss :&: ds) -> (SCons s ss) :&: (d :<# ds)
         
-- Note: when working with singletons, it is very important to use the
--       extra power of `case` pattern matching!
--
-- For instance, this won't work:
--
-- mkSomeHallway ((s :&: d) : sds) =
--   (SCons s ss) :&: (d :<# ds)
--  where
--   (ss :&: ds) = mkSomeHallway sds
--
-- yielding the following compiler error:
--
-- <- 
--        • Couldn't match type ‘fst1’ with ‘n20’
--          ‘fst1’ is a rigid type variable bound by
--            a pattern with constructor:
--              :&: :: forall s (a :: s ~> *) (fst :: s).
--                     Sing fst -> (a @@ fst) -> Sigma s a,
--            in
-- <-  a pattern binding
--            at /Users/a594349/Documents/Projects/HaskellMisc/singletons/Door4Final.hs:283:4-12
--          Expected type: Sing n20
--            Actual type: Sing fst1
--        • In the pattern: ss :&: ds
--          In a pattern binding: (ss :&: ds) = mkSomeHallway sds
   
-- <-       In an equation for ‘mkSomeHallway’:
--              mkSomeHallway ((s :&: d) : sds)
--                = (SCons s ss) :&: (d :<# ds)
--                where
--                    (ss :&: ds) = mkSomeHallway sds
--        |
--    283 |   (ss :&: ds) = mkSomeHallway sds
--        |    ^^
--    Fai
-- <- led, no modules loaded.
--    
