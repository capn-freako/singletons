-- For doing the exercises in Justin Le's "Fixed-Length Vector Types in Haskell (an Update for 2017)"
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   November 15, 2018
--
-- Copyright (c) 2018 David Banas; all rights reserved World wide.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Vec where

import Prelude hiding (replicate)

-- import Language.Haskell.TH
import Data.Singletons.TH

import qualified Data.Vector as V
import           GHC.TypeNats

-- import Data.Finite
import Data.Key                         (Zip(..))
import Data.Kind
-- import Data.Proxy

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
  deriving Show

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

replicate :: forall n a. KnownNat n => a -> Vec n a
replicate x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (natVal (Proxy @n))

-- generate :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
-- generate f = UnsafeMkVec $ V.generate l (f . fromIntegral)
--   where
--     l = fromIntegral (natVal (Proxy @n))

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f v = UnsafeMkVec $ V.map f (getVector v)

instance Functor (Vec n) where
  fmap = mapVec

instance Zip (Vec n) where
  zip (UnsafeMkVec u) (UnsafeMkVec v) = UnsafeMkVec $ V.zip u v
  
instance KnownNat n => Applicative (Vec n) where
  pure = replicate
  UnsafeMkVec f <*> UnsafeMkVec x = UnsafeMkVec $ V.zipWith ($) f x

-- Inductively defined data structures.
$(singletons [d|
  data Nat' = Z | S Nat'
    deriving Eq
  |])

data Fin :: Nat' -> Type where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)

deriving instance Show (Fin n)

data Vec' :: Nat' -> Type -> Type where
  VNil :: Vec' 'Z a
  (:+) :: a -> Vec' n a -> Vec' ('S n) a

instance Show a => Show (Vec' n a) where
  show = \case
    VNil    -> ""
    x :+ xs -> show x ++ " " ++ show xs

infixr 5 :+
  
generate_ :: Sing n -> (Fin n -> a) -> Vec' n a
generate_ s f = case s of
  SZ    -> VNil
  SS s' -> f FZ :+ generate_ s' (f . FS)

generate :: SingI n => (Fin n -> a) -> Vec' n a
generate = generate_ sing

fEnum :: Fin n -> Int
fEnum = \case
  FZ   -> 0
  FS n -> 1 + fEnum n

-- λ> print $ (generate fEnum :: Vec' Z Int)
--
-- λ> print $ (generate fEnum :: Vec' (S Z) Int)
-- 0
-- λ> print $ (generate fEnum :: Vec' (S (S Z)) Int)
-- 0 1 
-- λ> print $ (generate fEnum :: Vec' (S (S (S Z))) Int)
-- 0 1 2 

withVec :: [a] -> (forall n. Sing n -> Vec' n a -> r) -> r
withVec = \case
    []   -> \f -> f SZ VNil
    x:xs -> \f -> withVec xs $ \l ys ->
        f (SS l) (x :+ ys)

