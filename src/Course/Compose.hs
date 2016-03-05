{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional
-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  f <$> (Compose  c) = Compose $ ((<$>) f) <$> c

-- (Compose $ (Full (\x -> x + 1)) :. (Full (\x -> x - 2)) :. Empty :. Nil) <*> (Compose $ (Full 2) :. Empty :. (Full 4) :. Nil)
-- gives Compose [Full 3,Empty,Full 5,Full 0,Empty,Full 2,Empty,Empty,Empty]

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure =
    Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (Compose f) <*> (Compose c) = Compose $ (<*>) <$> f <*> c
    

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"

-- fun :: (Compose f g Int) -> Int -> Int
-- fun (Compose (f (g a))) x =
  -- 5
