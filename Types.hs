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

import Control.Monad.Trans.Class

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

-- Ex. 6.4-i
instance Functor Cont where
  fmap (f :: a -> b) (Cont (c :: forall r. (a -> r) -> r)) =
    Cont $ \ (g :: b -> r) -> (g . f . c) id

-- Ex. 6.4-ii
instance Applicative Cont where
  pure (x :: a) = Cont $ \ (f :: a -> r) -> f x
  Cont (f :: forall r. ((a -> b) -> r) -> r)
    <*> Cont (g :: forall r'. (a -> r') -> r') =
    Cont $ \ (h :: b -> r) -> h $ (f id) (g id)

-- Ex. 6.4-iii
instance Monad Cont where
  return  = pure
  c >>= f = f $ unCont c id

-- Ex. 6.4-iv
newtype ContT m a = ContT { runContT :: m (Cont a) }

instance Monad m => Functor (ContT m) where
  fmap f (ContT x) = ContT $ do
    c <- x
    return $ fmap f c
    
instance Monad m => Applicative (ContT m) where
  pure = ContT . return . pure
  ContT mg <*> ContT mx = ContT $ do
    g <- mg
    x <- mx
    return $ g <*> x
  
instance Monad m => Monad (ContT m) where
  return = pure
  -- ct >>= (f :: a -> ContT m b) = ContT $ do
  --   c <- runContT ct
  --   runContT $ f $ unCont c id
  ContT m >>= f = ContT $ m >>= \x -> runContT $ f $ unCont x id
  
instance MonadTrans ContT where
  -- lift ma = ContT $ do
  --   y <- ma
  --   return $ pure y
  lift m = ContT $ m >>= return . pure
  
-- Testing laws...
--
-- 1) lift . return = return
--
-- lift . return                 =  {definition of composition}
-- \x -> lift (return x)         =  {lift@ContT}
-- \x -> ContT $ return $ pure x =  {eta-reduction}
-- ContT . return . pure         =  {definition of return@ContT}
-- return
--
-- => Q.E.D.
--
-- 2) lift (m >>= f) = lift m >>= (lift . f)
--
-- lift (m >>= f)                            =  {definition of lift@ContT}
-- ContT $ m >>= f >>= \y -> return (pure y) =  {definition of composition}
-- ContT $ m >>= f >>= return . pure

-- (I'm stuck here, trying to connect the above and below.)

-- ContT $ m >>= runContT . (\x -> (ContT (f x >>= return . pure)))
--                                                            =  {eta expansion}
-- ContT $ m >>= \z -> runContT $ (\x -> (ContT (f x >>= return . pure))) z
--                                                            =  {function application}
-- ContT $ m >>= \z -> runContT $ (\x -> (ContT (f x >>= return . pure))) $ (\g -> g z) id
--                                                            =  {definition of pure@Cont}
-- ContT $ m >>= \z -> runContT $ (\x -> (ContT (f x >>= return . pure))) $ unCont (pure z) id
--                                                            =  {Monad laws (return x >>= f = f x)}
-- ContT $ m >>= \z -> return (pure z) >>=
--   \y -> runContT $ (\x -> (ContT (f x >>= return . pure))) $ unCont y id
--                                                            =  {definition of composition}
-- ContT $ m >>= return . pure >>=
--   \y -> runContT $ (\x -> (ContT (f x >>= return . pure))) $ unCont y id
--                                                            =  {definition of (>>=)@ContT}
-- (ContT $ m >>= return . pure)
--        >>= (\x -> (ContT (f x >>= return . pure)))         =  {definition of lift@ContT}
-- lift m >>= (\x -> (ContT (f x >>= return . pure)))         =  {definition of composition}
-- lift m >>= (\x -> (ContT (f x >>= \y -> return (pure y)))) =  {definition of lift@ContT}
-- lift m >>= (\x -> lift (f x))                              =  {definition of composition}
-- lift m >>= (lift . f)
