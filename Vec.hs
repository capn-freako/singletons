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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Vec where

import Prelude hiding (replicate)

import qualified Data.Vector as V
import           GHC.TypeNats

import Data.Finite
import Data.Key                         (Zip(..))
import Data.Proxy

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

generate :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
generate f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (natVal (Proxy @n))

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f v = UnsafeMkVec $ V.map f (getVector v)

instance Functor (Vec n) where
  fmap = mapVec

instance Zip (Vec n) where
  zip (UnsafeMkVec u) (UnsafeMkVec v) = UnsafeMkVec $ V.zip u v
  
instance KnownNat n => Applicative (Vec n) where
  pure = replicate
  UnsafeMkVec f <*> UnsafeMkVec x = UnsafeMkVec $ V.zipWith ($) f x

