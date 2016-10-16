module DisjointSet.Computation exposing (Computation, find, union, return, andThen, eval,  mapM, sequence, map)

{-| Composable "Computation" objects that handle threading the updated DisjointSet object for you.

# Definition
@docs Computation, find, union, eval, return

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
andThen callback (State fa) =
    State
        <| \set ->
            let
                ( a, newSet ) =
                    fa set

                (State k) =
                    callback a
            in
                k newSet


{-| -}
return : a -> Computation a
return a =
    State <| \set -> ( a, set )


{-| -}
eval : DisjointSet -> Computation a -> ( a, DisjointSet )
eval set (State s) =
    s set


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
                ( a, newSet ) =
                    fa set

                ( b, newSet2 ) =
                    fb newSet
            in
                ( f a b, newSet2 )
