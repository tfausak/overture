{-# LANGUAGE NoImplicitPrelude #-}

module Overture where

-- * Functions

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
