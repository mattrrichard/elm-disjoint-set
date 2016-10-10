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
    let
        ( set, f' ) = findSet x f
    in
        ( set.parentId, f' )


findSet : Int -> DisjointSet -> ( Set, DisjointSet )
findSet x (Forest arr) =
    let
        set =
            case Array.get x arr of
                Just s ->
                    s

                -- if an invalid id is asked for, return a set with a negative rank so it will
                -- never be chosen as the parent in a union operation
                Nothing ->
                    invalidSet x

        compress set id ( root, Forest arr ) =
            let
                arr' =
                    if root.parentId /= set.parentId then
                        Array.set id { set | parentId = root.parentId } arr
                    else
                        arr
            in
                ( root, Forest arr' )
    in
        if set.parentId == x then
            ( set, Forest arr )
        else
            findSet set.parentId (Forest arr)
                |> compress set x


{-| Join two subsets into a single subset.

    let
        set =
            union 0 1 (init 3)

        ( p0, set' ) =
            find 0 set

        ( p1, set'' ) =
            find 1 set'

        ( p1, set''' ) =
            find 2 set''
    in
        ( p0 == p1, p1 == p2 ) -- ( True, False )

In the above example, you can see that keeping up with the set as it updates
across several operations can start to get a little cumbersome. Check out the
`DisjointSet.Computation` module for some tools that help deal with this.
-}
union : Int -> Int -> DisjointSet -> DisjointSet
union x y f =
    let
        ( sx, f' ) = findSet x f

        ( sy, f'' ) = findSet y f'

        (Forest arr) = f''
    in
        if sx.parentId == sy.parentId then
            f''
        else
            let
                ( p, c ) =
                    if sx.rank >= sy.rank then
                        ( sx, sy )
                    else
                        ( sy, sx )

                arr' =
                    -- becase we always attach the smaller tree to the root
                    -- of the bigger tree, the size only grows if they were the
                    -- same size.  In that case, increment the parent tree's rank
                    if p.rank == c.rank then
                        Array.set p.parentId { p | rank = p.rank + 1 } arr
                    else
                        arr

                c' = { c | parentId = p.parentId }
            in
                -- since this only operates on tree roots,
                -- the parentId is the same as the index
                Forest (Array.set c.parentId c' arr')
