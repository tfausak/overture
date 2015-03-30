{- |
    Overture is an alternative to some of the "Prelude". It borrows ideas from
    <http://elm-lang.org Elm> and <http://fsharp.org F#>. It aims to provide a
    more readable set of functions in order to make Haskell easier to use.

    Overture does not export anything that conflicts with the "Prelude". To
    use it, simply import it.

    >>> import Overture
-}
module Overture where

-- * Identity function

{- |
    <https://en.wikipedia.org/wiki/Identity_function Identity function>.

    Use this function to always return the value it was given.

    >>> identity True
    True

    This is useful when constructing a function that returns another function.

    >>> let f x = if even x then recip else identity
    >>> f 2 10
    0.1
    >>> f 3 10
    10.0

    This is like the 'id' function from the "Prelude".
-}
identity :: a -> a
identity x = x

-- * Constant function

{- |
    <https://en.wikipedia.org/wiki/Constant_function Constant function>.

    Use this function to always return some value, regardless of what the
    other argument is.

    >>> always True False
    True

    This can be useful with higher-order functions. For example, this creates
    a list of three @True@s.

    >>> map (always True) [1 .. 3]
    [True,True,True]

    This is like the 'const' function from the "Prelude".
-}
always :: a -> b -> a
always x _ = x

-- * Function composition

{- |
    <https://en.wikipedia.org/wiki/Function_composition Function composition>.

    Use this function to combine two other functions. The result of
    @'compose' f g@ will be a new function that first applies @f@ and then
    applies @g@. In other words, @'compose' f g x@ is the same as @g (f x)@.
    For instance, the following example will first add one and then multiply
    by two.

    >>> let f = compose (+ 1) (* 2)
    >>> f 3
    8

    You can compose many functions together, but it quickly becomes unwieldy.
    This example does what the previous one did and then cubes the result.

    >>> let g = compose (compose (+ 1) (* 2)) (^ 3)
    >>> g 3
    512

    This is like the function form of the '.' operator from the "Prelude".
-}
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

{- |
    Left-associative 'compose' operator.

    Use this operator to combine two other functions in a more natural way
    than with 'compose'. The result of @f '.>' g@ will be a new function that
    first applies @f@ and then applies @g@. Here is the same example from
    above rewritten using this operator.

    >>> let f = (+ 1) .> (* 2)
    >>> f 3
    8

    When reading code, it is useful to pronounce this operator as "and then".
    So the above example could be read as: "Add one, /and then/ multiply by
    two".

    When composing many functions, it's easier to use this operator than
    'compose'. Compare this with the earlier example.

    >>> let g = (+ 1) .> (* 2) .> (^ 3)
    >>> g 3
    512

    This is like a flipped version of '.' operator from the "Prelude".
-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

{- |
    Right-associative 'compose' operator.

    Sometimes it is more convenient to combine functions in the opposite
    direction as '.>'. The result of @g '<.' f@ will be a new function that
    applies @g@ but first applies @f@. Here is the same example from before
    rewritten again.

    >>> let f = (* 2) <. (+ 1)
    >>> f 3
    8

    When reading code, it is useful to pronounce this operator as "but first".
    So the above example could be read as: "Multiply by two, /but first/ add
    one".

    Using this operator also leads to more readable code than using 'compose'
    with many functions.

    >>> let g = (^ 3) <. (* 2) <. (+ 1)
    >>> g 3
    512

    This is like the '.' operator from the "Prelude".
-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g

-- * Function application

{- |
    <https://en.wikipedia.org/wiki/Function_application Function application>.

    This function isn't usually necessary since @'apply' x f@ is the same as
    @f x@.

    >>> apply (apply 3 (+ 1)) (* 2)
    8

    However it can come in handy when working with higher-order functions.

    >>> map (apply 3) [(+ 1), (* 2)]
    [4,6]

    This is like the '$' operator from the "Prelude".
-}
apply :: a -> (a -> b) -> b
apply x f = f x

{- |
    Left-associative 'apply' operator.

    Since this operator has such low precedence, it can be used to remove
    parentheses in complicated expressions.

    >>> 3 |> (+ 1) |> (* 2)
    8

    When reading code, it is useful to pronounce this operator as "pipe into".
    So the above example can be read as: "Three /piped into/ plus one,
    /piped into/ times two".

    It can also be used with higher-order functions, although 'apply' might be
    clearer.

    >>> map (3 |>) [(+ 1), (* 2)]
    [4,6]

    This is like a flipped version of the '$' operator from the "Prelude".
-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

{- |
    Right-associative 'apply' operator.

    Like '|>', this operator also has low precedence. Use it to remove
    parentheses.

    >>> (* 2) <| (+ 1) <| 3
    8

    When reading code, it is useful to pronounce this operator as "pipe from".
    So the above example can be read as: "Times two /piped from/ plus one,
    /piped from/ 3".

    With higher-order functions, it can be a convenient alternative to
    @flip 'apply'@.

    >>> map (<| 3) [(+ 1), (* 2)]
    [4,6]

    This is like the '$' operator from the "Prelude".
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f

-- ** Strict function application

{- |
    Strict function application.

    This is the strict version of 'apply'. It evaluates its argument with
    'seq' before applying it to the given function. In other words,
    @'apply'' x f@ is the same as @x \`seq\` 'apply' x f@.

    >>> apply' undefined (const 0)
    *** Exception: Prelude.undefined

    This is like the '$!' operator from the "Prelude".
-}
apply' :: a -> (a -> b) -> b
apply' x f = x `seq` apply x f

{- |
    Left-associative 'apply'' operator.

    This is the strict version of the '|>' operator.

    >>> undefined !> const 0
    *** Exception: Prelude.undefined

    This is like a flipped version of the '$!' operator from the "Prelude".
-}
infixl 0 !>
(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

{- |
    Right-associative 'apply'' operator.

    This is the strict version of the '<!' operator.

    >>> const 0 <! undefined
    *** Exception: Prelude.undefined

    This is like the '$!' operator from the "Prelude".
-}
infixr 0 <!
(<!) :: (a -> b) -> a -> b
f <! x = apply' x f
