module Main (main) where

import Criterion.Main
import Overture

main :: IO ()
main = defaultMain
    [ bgroup "apply"
        [ bench "f x" <| whnf f x
        , bench "apply f x" <| whnf (apply x) f
        , bench "x |> f" <| whnf (x |>) f
        , bench "f <| x" <| whnf (<| x) f
        ]
    , bgroup "apply'"
        [ bench "seq x (f x)" <| whnf (seq x) (f x)
        , bench "apply' f x" <| whnf (apply' x) f
        , bench "x !> f" <| whnf (x !>) f
        , bench "f <! x" <| whnf (<! x) f
        ]
    , bgroup "compose"
        [ bench "f . g" <| whnf (f .) g
        , bench "compose f g" <| whnf (compose f) g
        , bench "f .> g" <| whnf (f .>) g
        , bench "g <. f" <| whnf (<. g) f
        ]
    ]

x :: Int
x = 0

f :: Int -> Int
f = id

g :: Int -> Int
g = id
