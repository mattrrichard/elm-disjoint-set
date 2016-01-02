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


find : Int -> DisjointSet -> (Int, DisjointSet)
find x f =
    let
        (set, f') = findSet x f
    in
        (set.parentId, f')


findSet : Int -> DisjointSet -> (Set, DisjointSet)
findSet x (Forest arr) =
    let
        safeGet i arr =
            case Array.get i arr of
                Just set -> set
                -- if an invalid id is asked for, just return a new set
                -- garbage in, garbage out. not my pig/farm
                Nothing -> initSet i

        set =
            safeGet x arr

        compress set id (root, Forest arr) =
            let
                set' = { set | parentId = root.parentId }
                arr' =
                    if root.parentId /= set.parentId then
                        Array.set id set' arr
                    else
                        arr
            in
                (root, Forest arr')
    in
        if set.parentId == x then
            (set, Forest arr)
        else
            findSet set.parentId (Forest arr)
            |> compress set x


union : Int -> Int -> DisjointSet -> DisjointSet
union x y f =
    let
        (sx, f') = findSet x f
        (sy, f'') = findSet y f'

        (parent, child) =
            if sx.rank > sy.rank then
                (sx, sy)
            else
                (sy, sx)

        setParent (Forest arr) p c =
            let
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
                Array.set c.parentId c' arr'

    in
        Forest (setParent f'' parent child)

