module DisjointSet
    ( DisjointSet
    , init
    , find
    , union
    ) where


import Array exposing (Array)


type DisjointSet =
    Forest (Array Set)

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


find : DisjointSet -> Int -> (Int, DisjointSet)
find f x =
    let
        (set, f') = findSet f x
    in
        (set.parentId, f')

findSet : DisjointSet -> Int -> (Set, DisjointSet)
findSet f x =
    Debug.crash "TODO"


union : DisjointSet -> Int -> Int -> DisjointSet
union f x y =
    let
        (sx, f') = findSet f x
        (sy, f'') = findSet f' y

        (parent, child) =
            if sx.rank < sy.rank then
                (sx, sy)
            else
                (sy, sx)

        setParent (Forest arr) p c =
            let
                arr' =
                    if p.rank == c.rank then
                        Array.set p.parentId { p | rank = p.rank + 1 } arr
                    else
                       arr

                c' = { c | parentId = p.parentId }
            in
                Array.set c.parentId c' arr'

    in
        Forest (setParent f'' parent child)

