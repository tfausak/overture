module Overture where

{- |
    >>> let f = compose (+ 1) (* 2)
    >>> f 3
    8

    >>> let g = compose (compose (+ 1) (* 2)) (^ 3)
    >>> g 3
    512
-}
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

{- |
    >>> let f = (+ 1) .> (* 2)
    >>> f 3
    8

    >>> let g = (+ 1) .> (* 2) .> (^ 3)
    >>> g 3
    512
-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

{- |
    >>> let f = (* 2) <. (+ 1)
    >>> f 3
    8

    >>> let g = (^ 3) <. (* 2) <. (+ 1)
    >>> g 3
    512
-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g

{- |
    >>> apply 3 (+ 1)
    4

    >>> map (apply 3) [(+ 1), (* 2)]
    [4,6]
-}
apply :: a -> (a -> b) -> b
apply x f = f x

{- |
    >>> 3 |> (+ 1)
    4

    >>> map (3 |>) [(+ 1), (* 2)]
    [4,6]
-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

{- |
    >>> (+ 1) <| 3
    4

    >>> map (<| 3) [(+ 1), (* 2)]
    [4,6]
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f
