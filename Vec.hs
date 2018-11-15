-- For doing the exercises in Justin Le's "Fixed-Length Vector Types in Haskell (an Update for 2017)"
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   November 15, 2018
--
-- Copyright (c) 2018 David Banas; all rights reserved World wide.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Vec where

import qualified Data.Vector as V
import           GHC.TypeNats

import Data.Proxy

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
  deriving Show

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

