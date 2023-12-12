interface Matrix
    exposes [
        Point,
        Matrix,
        findFirstIndex,
        get,
        len,
        map,
        mapWithIndex,
        toStr,
    ]
    imports []

    
Point : {x: Nat, y: Nat}

Matrix a : List (List a)


findFirstIndex : Matrix elem, (elem -> Bool) -> Result Point [NotFound]
findFirstIndex = \matrix, predicate ->
    List.walkWithIndex matrix (Err NotFound) \found, line, y ->
        if found != Err NotFound
            then found
            else List.findFirstIndex line predicate
                |> Result.map \x -> {x, y}

get : Matrix a, Point -> Result a [OutOfBoundsY, OutOfBoundsX]
get = \matrix, {x, y} ->
    lres = List.get matrix y
    Result.try lres (\line ->
        List.get line x |> Result.mapErr \_ -> OutOfBoundsX)
        |> Result.mapErr \_ -> OutOfBoundsY

len : Matrix a -> Point
len = \matrix ->
    when matrix is
        [] -> {x: 0, y: 0}
        [line, ..] -> {x: List.len line, y: List.len matrix}

map : Matrix a, (a -> b) -> Matrix b
map = \matrix, fn ->
    List.map matrix \line ->
        List.map line fn

mapWithIndex : Matrix a, (a, Point -> b) -> Matrix b
mapWithIndex = \matrix, fn ->
    List.mapWithIndex matrix \line, y ->
        List.mapWithIndex line \elem, x ->
            fn elem {x, y}

toStr : Matrix Str -> Str
toStr = \matrix ->
    List.map matrix (\line -> Str.joinWith line "")
        |> Str.joinWith "\n"