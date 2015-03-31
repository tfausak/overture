module Main (main) where

import Criterion.Main
import Overture

main :: IO ()
main = defaultMain
    [ bgroup "apply"
        [ bench "id x" <| whnf id x
        , bench "apply id x" <| whnf (apply x) id
        , bench "x |> id" <| whnf (x |>) id
        , bench "id <| x" <| whnf (<| x) id
        ]
    , bgroup "apply'"
        [ bench "seq x (id x)" <| whnf (seq x) (id x)
        , bench "apply' id x" <| whnf (apply' x) id
        , bench "x !> id" <| whnf (x !>) id
        , bench "id <! x" <| whnf (<! x) id
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
