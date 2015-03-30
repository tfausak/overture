{-# LANGUAGE Safe #-}

{- |
    Overture is an alternative to some of the "Prelude". It borrows ideas from
    <http://elm-lang.org Elm> and <http://fsharp.org F#>. It aims to provide a
    more readable set of functions in order to make Haskell easier to use. It
    does not export anything that conflicts with the "Prelude". To use it,
    simply import it.

    >>> import Overture
-}
module Overture where

-- * Identity function

{- |
    <https://en.wikipedia.org/wiki/Identity_function Identity function>.
    This is like the 'id' function from the "Prelude".

    This function returns the value it was given. The result of @'identity' x@
    is @x@.

    >>> identity True
    True

    This is useful when constructing functions that return other functions.

    >>> let f x = if x then recip else identity
    >>> f False 10
    10.0
    >>> f True 10
    0.1
-}
identity :: a -> a
identity x = x

-- * Constant function

{- |
    <https://en.wikipedia.org/wiki/Constant_function Constant function>.
    This is like the 'const' function from the "Prelude".

    This function takes two arguments and always returns the first. In other
    words, it ignores its second argument. The result of @'always' x y@ is @x@.

    >>> always True undefined
    True

    This can be useful with higher-order functions like 'map'.

    >>> map (always True) [1 .. 3]
    [True,True,True]
-}
always :: a -> b -> a
always x _ = x

-- * Function composition

{- |
    <https://en.wikipedia.org/wiki/Function_composition Function composition>.
    This is like the '.' operator from the "Prelude".

    This function combines two other functions. The result of @'compose' f g@
    is a new function the applies @f@ first and then applies @g@. In other
    words, @'compose' f g x@ is the same as @g (f x)@.

    >>> let f = compose succ recip
    >>> f 9
    0.1

    Composing many functions together quickly becomes unwieldy. Use '.>' or
    '<.' instead.

    >>> let g = succ `compose` recip `compose` negate
    >>> g 9
    -0.1
-}
compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \ x -> g (f x)

{- |
    Left-associative 'compose' operator.
    This is like a flipped version of '.' operator from the "Prelude".

    This operator combines two other functions more naturally than 'compose'.
    The result of @f '.>' g@ is a new function that applies @f@ first and then
    applies @g@.

    >>> let f = succ .> recip
    >>> f 9
    0.1

    When reading code, pronounce this operator as "and then". So the above
    example could be read as: "Add one, /and then/ take the reciprocal."

    Thanks to its high precedence, composing many functions together is easy
    with this operator.

    >>> let g = succ .> recip .> negate
    >>> g 9
    -0.1
-}
infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = compose f g

{- |
    Right-associative 'compose' operator.
    This is like the '.' operator from the "Prelude".

    Sometimes it is more convenient to combine functions from right to left.
    The result of @g '<.' f@ is a new function that applies @g@ but first
    applies @f@.

    >>> let f = recip <. succ
    >>> f 9
    0.1

    Pronounce this operator as "but first" when reading code. The example above
    could be read as: "Take the reciprocal, /but first/ add one."

    Composing many functions together is easy with this operator thanks to its
    high precedence.

    >>> let g = negate <. recip <. succ
    >>> g 9
    -0.1
-}
infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
g <. f = compose f g

-- * Function application

{- |
    <https://en.wikipedia.org/wiki/Function_application Function application>.
    This is like the '$' operator from the "Prelude".

    This function applies an argument to function.

    >>> apply 4 succ
    5

    Using this function to apply many arguments is cumbersome. Use '|>' or '<|'
    instead.

    >>> 4 `apply` succ `apply` recip
    0.2

    This function usually isn't necessary since @'apply' x f@ is the same as
    @f x@. However it can come in handy when working with higher-order
    functions.

    >>> map (apply 4) [succ, recip]
    [5.0,0.25]
-}
apply :: a -> (a -> b) -> b
apply x f = f x

{- |
    Left-associative 'apply' operator.
    This is like a flipped version of the '$' operator from the "Prelude".

    This operator applies an argument to a function. The result of @x '|>' f@
    is @f x@.

    >>> 4 |> succ
    5

    Since this operator has such low precedence, it can be used to remove
    parentheses in complicated expressions.

    >>> 4 |> succ |> recip
    0.2

    When reading code, pronounce this operator as "pipe into". So the above
    example can be read as: "Four /piped into/ plus one, /piped into/ the
    reciprocal."

    It can also be used with higher-order functions, although 'apply' might be
    clearer.

    >>> map (4 |>) [succ, recip]
    [5.0,0.25]
-}
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

{- |
    Right-associative 'apply' operator.
    This is like the '$' operator from the "Prelude".

    Sometimes it is more convenient to apply arguments right to left. The
    result of @f '<|' x@ is @f x@.

    >>> succ <| 4
    5

    Like '|>', this operator has low precedence so it can be used to remove
    parentheses.

    >>> recip <| succ <| 4
    0.2

    Pronounce this operator as "pipe from" when reading code. The example above
    can be read as: "The reciprocal /piped from/ plus one, /piped from/ five."

    This operator is a convenient alternative to @flip 'apply'@.

    >>> map (<| 4) [succ, recip]
    [5.0,0.25]
-}
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = apply x f

-- ** Strict function application

{- |
    Strict function application.
    This is like the '$!' operator from the "Prelude".

    This is the strict version of 'apply'. It evaluates its argument with
    'seq' before applying it to the given function. In other words,
    @'apply'' x f@ is the same as @x \`seq\` 'apply' x f@.

    >>> apply' undefined (always 0)
    *** Exception: Prelude.undefined
-}
apply' :: a -> (a -> b) -> b
apply' x f = x `seq` apply x f

{- |
    Left-associative 'apply'' operator.
    This is like a flipped version of the '$!' operator from the "Prelude".

    This is the strict version of the '|>' operator.

    >>> undefined !> always 0
    *** Exception: Prelude.undefined
-}
infixl 0 !>
(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

{- |
    Right-associative 'apply'' operator.
    This is like the '$!' operator from the "Prelude".

    This is the strict version of the '<!' operator.

    >>> always 0 <! undefined
    *** Exception: Prelude.undefined
-}
infixr 0 <!
(<!) :: (a -> b) -> a -> b
f <! x = apply' x f
