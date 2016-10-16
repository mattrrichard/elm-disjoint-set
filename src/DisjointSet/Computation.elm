module DisjointSet.Computation exposing (Computation, find, union, return, andThen, eval, mapM, sequence, map)

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


{-| Create a computation that finds which subset the given id's set belongs to. -}
find : Int -> Computation Int
find id =
    State (DSet.find id)


{-| Create a computation that unions the two given sets -}
union : Int -> Int -> Computation ()
union a b =
    State <| \set -> ( (), DSet.union a b set )


{-| Chain computations together where the second may depend on the result of the first -}
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


{-| Create a computation that always returns the value given.  Frequently used to end an `andThen` chain. -}
return : a -> Computation a
return a =
    State <| \set -> ( a, set )


{-| Run a computation -}
eval : DisjointSet -> Computation a -> ( a, DisjointSet )
eval set (State s) =
    s set


{-| Create a computation that runs each of a list of computations in sequence -}
sequence : List (Computation a) -> Computation (List a)
sequence =
    List.foldr (map2 (::)) (return [])


{-| -}
mapM : (a -> Computation b) -> List a -> Computation (List b)
mapM k =
    sequence << List.map k


{-| Transform the result of a computation -}
map : (a -> b) -> Computation a -> Computation b
map f a =
    a |> andThen (f >> return)


-- I think I'm going to leave this internal for now because I can't think of any
-- reason you should need it where `andThen` wouldn't be more clear anyway
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
