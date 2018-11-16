-- For doing the exercises in Sandy McGuire's "Thinking With Types"
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

module Types where

-- import Prelude hiding (replicate)

-- import qualified Data.Vector as V
-- import           GHC.TypeNats

-- import Data.Finite
-- import Data.Key                         (Zip(..))
-- import Data.Proxy

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

-- Ex 6.4-i
instance Functor Cont where
  fmap (f :: a -> b) (Cont (c :: forall r. (a -> r) -> r)) =
    Cont $ \ (g :: b -> r) -> (g . f . c) id

-- Ex. 6.4-ii
instance Applicative Cont where
  pure (x :: a) = Cont $ \ (f :: a -> r) -> f x
  Cont (f :: forall r. ((a -> b) -> r) -> r)
    <*> Cont (g :: forall r'. (a -> r') -> r') =
    Cont $ \ (h :: b -> r) -> h $ (f id) (g id)

