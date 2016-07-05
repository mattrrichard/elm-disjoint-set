module Computation exposing (Computation, find, union, return, andThen, eval, eval', mapM, sequence, map)

import DisjointSet as DSet exposing (DisjointSet)


type Computation a
    = State (DisjointSet -> ( a, DisjointSet ))


find : Int -> Computation Int
find id =
    State (DSet.find id)


union : Int -> Int -> Computation ()
union a b =
    State <| \set -> ( (), DSet.union a b set )


andThen : Computation a -> (a -> Computation b) -> Computation b
andThen (State s) k =
    State
        <| \set ->
            let
                ( a, set' ) =
                    s set

                (State k') =
                    k a
            in
                k' set'


return : a -> Computation a
return a =
    State <| \set -> ( a, set )


eval : Computation a -> DisjointSet -> ( a, DisjointSet )
eval (State s) set =
    s set


eval' : DisjointSet -> Computation a -> ( a, DisjointSet )
eval' =
    (flip eval)



sequence : List (Computation a) -> Computation (List a)
sequence =
    let
        k m m' =
            m `andThen` \x ->
            m' `andThen` \xs ->
                return (x :: xs)
    in
        List.foldr k (return [])


mapM : (a -> Computation b) -> List a -> Computation (List b)
mapM k =
    sequence << List.map k


map : (a -> b) -> Computation a -> Computation b
map f a =
    a `andThen` (f >> return)
