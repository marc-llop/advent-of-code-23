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
        cardinalAdjacentPoints,
        manhattanDistance,
        cartesianProduct,
        walkColumn,
        getColumn,
        allColumn,
        anyColumn,
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

get : Matrix a, Point -> Result a [OutOfBounds]
get = \matrix, {x, y} ->
    lres = List.get matrix y
    Result.try lres (\line ->
        List.get line x |> Result.mapErr \_ -> OutOfBounds)
        |> Result.mapErr \_ -> OutOfBounds

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


cardinalAdjacentPoints : Matrix a, Point -> {
    top: Result Point [OutOfBounds],
    bottom: Result Point [OutOfBounds],
    left: Result Point [OutOfBounds],
    right: Result Point [OutOfBounds],
}
cardinalAdjacentPoints = \matrix, {x, y} ->
    matrixLength = Matrix.len matrix
    matrixLast = {x: matrixLength.x - 1, y: matrixLength.y - 1}
    top = if y > 0 
        then Ok {x, y: y - 1}
        else Err OutOfBounds
    bottom = if y < matrixLast.y
        then Ok {x, y: y + 1}
        else Err OutOfBounds
    left = if x > 0 
        then Ok {x: x - 1, y}
        else Err OutOfBounds
    right = if x < matrixLast.x 
        then Ok {x: x + 1, y}
        else Err OutOfBounds
    {top, bottom, left, right}

manhattanDistance : Point, Point -> Nat
manhattanDistance = \a, b ->
    (Num.absDiff a.x b.x) + (Num.absDiff a.y b.y)

cartesianProduct : List a, List b -> List (a, b)
cartesianProduct = \xs, ys ->
    List.joinMap xs \x ->
        List.map ys \y -> (x, y)

walkColumn : Matrix elem, Nat, state, (state, elem -> state) -> Result state [OutOfBounds]
walkColumn = \matrix, column, state, fn ->
    List.walk matrix (Ok state) \accResult, row ->
        elem <- List.get row column |> Result.try
        Result.map accResult \acc -> (fn acc elem)

getColumn : Matrix elem, Nat -> Result (List elem) [OutOfBounds]
getColumn = \matrix, columnIndex ->
    walkColumn matrix columnIndex [] \column, elem -> List.append column elem

allColumn : Matrix elem, Nat, (elem -> Bool) -> Bool
allColumn = \matrix, column, predicate ->
    walkColumn matrix column Bool.false (\acc, elem -> acc && predicate elem)
        |> Result.withDefault Bool.false

anyColumn : Matrix elem, Nat, (elem -> Bool) -> Bool
anyColumn = \matrix, column, predicate ->
    walkColumn matrix column Bool.false (\acc, elem -> acc || predicate elem)
        |> Result.withDefault Bool.false