module DisjointSet exposing (DisjointSet, init, find, union)

{-| An implementation of the Disjoint Set data structure

# Disjoint Set

@docs DisjointSet

# Operations

@docs init, find, union

-}

import Array exposing (Array)


{-| Data structure containing a set of (initially independent) subsets.
-}
type DisjointSet
    = Forest (Array Set)


type alias Set =
    { rank : Int
    , parentId : Int
    }


{-| Create a new DisjointSet of the specified size. The subsets are assigned
integer ids starting at 0.

    init 8 -- create a disjoint set of 8 independent subsets numbered 0-7
-}
init : Int -> DisjointSet
init n =
    Forest (Array.initialize n initSet)


initSet : Int -> Set
initSet id =
    { rank = 0
    , parentId = id
    }



-- Create a set with a negative rank. Used to prevent a set with an
-- out-of-range id from being selected as the parent tree in a union.


invalidSet : Int -> Set
invalidSet id =
    { rank = -1
    , parentId = id
    }


mapFst : (a -> b) -> ( a, c ) -> ( b, c )
mapFst f ( x, y ) =
    ( f x, y )


mapSnd : (a -> b) -> ( c, a ) -> ( c, b )
mapSnd f ( x, y ) =
    ( x, f y )


{-| Find which subset the given id's set belongs to.

    find 2 (init 5) |> fst -- 2
    find 2 (union 3 2 (init 5)) |> fst -- 3

Note that this function returns a tuple with the first element containg the
result of the find and the second a new DisjointSet object. This is because
`find` performs an optimization which changes some internal structure (without
changing the sets themselves).

For simplicity, if the id asked for is not valid, the result is always that same
value.

    find 100 (init 5) |> fst -- 100

This behavior may change in the future, but I wanted to avoid muddying the api
with a `Maybe` result here.

-}
find : Int -> DisjointSet -> ( Int, DisjointSet )
find x f =
    mapFst .parentId (findSet x f)


findSet : Int -> DisjointSet -> ( Set, DisjointSet )
findSet id (Forest array) =
    let
        set =
            case Array.get id array of
                Just s ->
                    s

                -- if an invalid id is asked for, return a set with a negative rank so it will
                -- never be chosen as the parent in a union operation
                Nothing ->
                    invalidSet id

        compress ( root, Forest arr ) =
            let
                newArr =
                    if root.parentId /= set.parentId then
                        Array.set id { set | parentId = root.parentId } arr
                    else
                        arr
            in
                ( root, Forest newArr )
    in
        if set.parentId == id then
            ( set, Forest array )
        else
            findSet set.parentId (Forest array)
                |> compress


{-| Join two subsets into a single subset.

    let
        set =
            union 0 1 (init 3)

        ( p0, set2 ) =
            find 0 set

        ( p1, set3 ) =
            find 1 set2

        ( p1, set4 ) =
            find 2 set3
    in
        ( p0 == p1, p1 == p2 ) -- ( True, False )

In the above example, you can see that keeping up with the set as it updates
across several operations can start to get a little cumbersome. Check out the
`DisjointSet.Computation` module for some tools that help deal with this.
-}
union : Int -> Int -> DisjointSet -> DisjointSet
union x y f =
    let
        ( sx, f2 ) =
            findSet x f

        ( sy, f3 ) =
            findSet y f2

        (Forest arr) =
            f3
    in
        if sx.parentId == sy.parentId then
            f3
        else
            let
                ( parent, child ) =
                    if sx.rank >= sy.rank then
                        ( sx, sy )
                    else
                        ( sy, sx )

                newArr =
                    -- becase we always attach the smaller tree to the root
                    -- of the bigger tree, the size only grows if they were the
                    -- same size.  In that case, increment the parent tree's rank
                    if parent.rank == child.rank then
                        Array.set parent.parentId { parent | rank = parent.rank + 1 } arr
                    else
                        arr

                newChild =
                    { child | parentId = parent.parentId }
            in
                -- since this only operates on tree roots,
                -- the parentId is the same as the index
                Forest (Array.set child.parentId newChild newArr)
