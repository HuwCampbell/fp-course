{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Contravariant

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  f <$> Compose wrapped =
    Compose ((f <$>) <$> wrapped)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure

  Compose f <*> Compose wrapped =
    Compose (((<*>) <$> f) <*> wrapped)

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance (Functor f, Contravariant g) => Contravariant (Compose f g) where
  f >$< Compose fg =
    Compose ((f >$<) <$> fg)
