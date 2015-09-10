{-# LANGUAGE NoImplicitPrelude #-}

module Overture where

import qualified Prelude as Prelude

-- * Functions

always :: a -> (b -> a)
always x = \ _ -> x

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \ x y -> f y x

-- * Types

-- ** Boolean

data Boolean
    = False
    | True

otherwise :: Boolean
otherwise = True

boolean :: a -> a -> Boolean -> a
boolean x y p = case p of
    False -> y
    True -> x

and :: Boolean -> Boolean -> Boolean
and x y = case x of
    False -> False
    True -> y

(&&) :: Boolean -> Boolean -> Boolean
x && y = and x y

or :: Boolean -> Boolean -> Boolean
or x y = case x of
    False -> y
    True -> True

(||) :: Boolean -> Boolean -> Boolean
x || y = or x y

not :: Boolean -> Boolean
not x = case x of
    False -> True
    True -> False

-- ** Maybe

data Maybe a
    = Nothing
    | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f my = case my of
    Nothing -> x
    Just y -> f y

isNothing :: Maybe a -> Boolean
isNothing mx = case mx of
    Nothing -> True
    Just _ -> False

isJust :: Maybe a -> Boolean
isJust mx = not (isNothing mx)

-- ** Either

data Either a b
    = Left a
    | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g ex = case ex of
    Left x -> f x
    Right x -> g x

isLeft :: Either a b -> Boolean
isLeft ex = case ex of
    Left _ -> True
    Right _ -> False

isRight :: Either a b -> Boolean
isRight ex = not (isLeft ex)

-- * Classes

-- ** Categories

class Category g where
    identity :: g a a

    compose :: g a b -> g b c -> g a c

instance Category (->) where
    identity x = x

    compose f g = \ x -> g (f x)

(.>) :: (Category g) => g a b -> g b c -> g a c
f .> g = compose f g

(<.) :: (Category g) => g b c -> g a b -> g a c
f <. g = compose g f

-- ** Monoids

class Monoid m where
    empty :: m a

    append :: m a -> m a -> m a

instance Monoid [] where
    empty = []

    append xs ys = case xs of
        [] -> ys
        (z : zs) -> z : append zs ys

(++) :: (Monoid m) => m a -> m a -> m a
xs ++ ys = append xs ys

-- ** Functors

class Functor f where
    map :: (a -> b) -> f a -> f b

instance Functor ((->) a) where
    map = (<.)

instance Functor [] where
    map f xs = case xs of
        [] -> []
        (y : ys) -> f y : map f ys

instance Functor Maybe where
    map f mx = case mx of
        Nothing -> Nothing
        Just x -> Just (f x)

instance Functor (Either a) where
    map f ex = case ex of
        Left x -> Left x
        Right x -> Right (f x)

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = map f x

($>) :: (Functor f) => f a -> b -> f b
x $> y = map (always y) x

(<$) :: (Functor f) => a -> f b -> f a
x <$ y = map (always x) y

-- ** Applicatives

class (Functor p) => Applicative p where
    pure :: a -> p a

    apply :: p (a -> b) -> p a -> p b

instance Applicative ((->) a) where
    pure x = always x

    apply f g = \ x -> f x (g x)

instance Applicative [] where
    pure x = [x]

    apply fs xs = case (fs, xs) of
        ([], _) -> []
        (_, []) -> []
        (g : gs, y : ys) -> g y : apply gs ys

instance Applicative Maybe where
    pure = Just

    apply mf x = maybe Nothing (\ f -> map f x) mf

instance Applicative (Either a) where
    pure = Right

    apply ef x = either Left (\ f -> map f x) ef

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

instance Monad [] where
    bind xs f = case xs of
        [] -> []
        (y : ys) -> f y ++ bind ys f

instance Monad Maybe where
    bind mx f = join (map f mx)

instance Monad (Either a) where
    bind ex f = join (map f ex)

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
x >>= f = bind x f

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
f =<< x = bind x f

(>>) :: (Monad m) => m a -> m b -> m b
x >> y = bind x (always y)

(<<) :: (Monad m) => m a -> m b -> m a
x << y = bind y (always x)

join :: (Monad m) => m (m a) -> m a
join x = bind x identity
