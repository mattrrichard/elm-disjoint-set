module DisjointSet exposing (DisjointSet, init, find, union)

import Array exposing (Array)


type DisjointSet
    = Forest (Array Set)


type alias Set =
    { rank : Int
    , parentId : Int
    }


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
