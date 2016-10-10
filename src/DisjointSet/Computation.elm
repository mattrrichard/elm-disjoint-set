module DisjointSet.Computation exposing (Computation, find, union, return, andThen, eval, eval', mapM, sequence, map)

{-| Composable "Computation" objects that handle threading the updated DisjointSet object for you.

# Definition
@docs Computation, find, union, eval, eval', return

# Composition
@docs andThen, map

# List Helpers
@docs mapM, sequence

-}

import DisjointSet as DSet exposing (DisjointSet)


{-| Represents a series of operations over a DisjointSet object producing a result of type a.
That is, a `Computation Int` produces an `Int` when evaluated.

To actually evaluate a computation, see the [`eval`](#eval) function.

-}
type Computation a
    = State (DisjointSet -> ( a, DisjointSet ))


{-| -}
find : Int -> Computation Int
find id =
    State (DSet.find id)


{-| -}
union : Int -> Int -> Computation ()
union a b =
    State <| \set -> ( (), DSet.union a b set )


{-| -}
andThen : (a -> Computation b) -> Computation a -> Computation b
andThen k (State s) =
    State
        <| \set ->
            let
                ( a, set' ) =
                    s set

                (State k') =
                    k a
            in
                k' set'


{-| -}
return : a -> Computation a
return a =
    State <| \set -> ( a, set )


{-| -}
eval : Computation a -> DisjointSet -> ( a, DisjointSet )
eval (State s) set =
    s set


{-| -}
eval' : DisjointSet -> Computation a -> ( a, DisjointSet )
eval' =
    (flip eval)


{-| -}
sequence : List (Computation a) -> Computation (List a)
sequence =
    List.foldr (map2 (::)) (return [])


{-| -}
mapM : (a -> Computation b) -> List a -> Computation (List b)
mapM k =
    sequence << List.map k


{-| -}
map : (a -> b) -> Computation a -> Computation b
map f a =
    a |> andThen (f >> return)


{-| -}
map2 : (a -> b -> c) -> Computation a -> Computation b -> Computation c
map2 f (State fa) (State fb) =
    -- Part of me hates implementing this at this level since it technically
    -- could be done in terms of `andThen`, but it gets real ugly, real fast
    State
        <| \set ->
            let
                ( a, set' ) =
                    fa set

                ( b, set'' ) =
                    fb set'
            in
                ( f a b, set'' )
