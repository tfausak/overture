{-# LANGUAGE NoImplicitPrelude #-}

module Overture where

import qualified Prelude as Prelude

-- * Functions

always :: a -> (b -> a)
always x = \ _ -> x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \ x y -> f y x

-- * Classes

-- ** Categories

class Category g where
    identity :: g a a

    compose :: g a b -> g b c -> g a c

instance Category (->) where
    identity x = x

    compose f g = \ x -> g (f x)

(.>) :: (Category g) => g a b -> g b c -> g a c
(.>) = compose

(<.) :: (Category g) => g b c -> g a b -> g a c
(<.) = flip (.>)

-- ** Functors

class Functor f where
    map :: (a -> b) -> f a -> f b

instance Functor ((->) a) where
    map = (<.)

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = map

($>) :: (Functor f) => f a -> b -> f b
x $> y = always y <$> x

(<$) :: (Functor f) => a -> f b -> f a
(<$) = flip ($>)

-- ** Applicatives

class (Functor p) => Applicative p where
    pure :: a -> p a

    apply :: p (a -> b) -> p a -> p b

instance Applicative ((->) a) where
    pure x = always x

    apply f g = \ x -> f x (g x)

(<*>) :: (Applicative p) => p (a -> b) -> p a -> p b
(<*>) = apply

(*>) :: (Applicative p) => p a -> p b -> p b
x *> y = (x $> identity) <*> y

(<*) :: (Applicative p) => p a -> p b -> p a
(<*) = flip (*>)

-- ** Monads

class (Applicative m) => Monad m where
    bind :: m a -> (a -> m b) -> m b

    fail :: Prelude.String -> m a
    fail x = Prelude.error x

instance Monad ((->) a) where
    bind f g = \ x -> g (f x) x

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
(>>=) = bind

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

(>>) :: (Monad m) => m a -> m b -> m b
x >> y = x >>= always y

(<<) :: (Monad m) => m a -> m b -> m a
(<<) = flip (>>)
