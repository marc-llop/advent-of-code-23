## # Matrix
## A common set of utility functions to deal with Lists of Lists.
interface Matrix
    exposes [
        Point,
        Matrix,
        findFirstIndex,
        get,
        set,
        update,
        len,
        map,
        map2,
        mapWithIndex,
        walk,
        walkWithIndex,
        toStr,
        cardinalAdjacentPoints,
        manhattanDistance,
        cartesianProduct,
        countIf,
        walkColumn,
        transpose,
        turnRight,
        mapColumnsWithIndex,
        walkColumnsWithIndex,
        getColumn,
        setColumn,
        allColumn,
        anyColumn,
    ]
    imports []

## A bidimensional coordinate. It can represent a point in a bidimensional structure
## or also a bidimensional size. It can be OutOfBounds.
Point : {x: Nat, y: Nat}

## A List of Lists. Alias provided for convenience.
Matrix a : List (List a)

## Returns the first point, in latin reading order (left to right and then top to bottom),
## at which the first element that satisfies a predicate can be found.
## If no satisfying element is found, an `Err NotFound` is returned.
findFirstIndex : Matrix elem, (elem -> Bool) -> Result Point [NotFound]
findFirstIndex = \matrix, predicate ->
    List.walkWithIndex matrix (Err NotFound) \found, line, y ->
        if found != Err NotFound
            then found
            else List.findFirstIndex line predicate
                |> Result.map \x -> {x, y}

## Returns an element from a matrix at the given point.
## Returns `Err OutOfBounds` if the given point exceeds the matrix's length in any dimension.
get : Matrix a, Point -> Result a [OutOfBounds]
get = \matrix, {x, y} ->
    lres = List.get matrix y
    Result.try lres (\line ->
        List.get line x |> Result.mapErr \_ -> OutOfBounds)
        |> Result.mapErr \_ -> OutOfBounds

## Replaces the element at the given point with a replacement.
## ```
## Matrix.set [["a", "b"], ["c", "d"]] {x: 0, y: 1} "B"
## ```
## If the given point is outside the bounds of the matrix, returns the original matrix unmodified.
set : Matrix a, Point, a -> Matrix a
set = \matrix, {x, y}, elem ->
    List.update matrix y \row ->
        List.set row x elem

## 
update : Matrix a, Point, (a -> a) -> Matrix a
update = \matrix, {x, y}, fn ->
    List.update matrix y \row ->
        List.update row x fn

len : Matrix a -> Point
len = \matrix ->
    when matrix is
        [] -> {x: 0, y: 0}
        [line, ..] -> {x: List.len line, y: List.len matrix}

map : Matrix a, (a -> b) -> Matrix b
map = \matrix, fn ->
    List.map matrix \line ->
        List.map line fn

map2 : Matrix a, Matrix b, (a, b -> c) -> Matrix c
map2 = \a, b, fn ->
    List.map2 a b \lineA, lineB ->
        List.map2 lineA lineB fn

mapWithIndex : Matrix a, (a, Point -> b) -> Matrix b
mapWithIndex = \matrix, fn ->
    List.mapWithIndex matrix \line, y ->
        List.mapWithIndex line \elem, x ->
            fn elem {x, y}

walk : Matrix elem, state, (state, elem -> state) -> state
walk = \matrix, state, fn ->
    List.walk matrix state \acc, row ->
        List.walk row acc fn

walkWithIndex : Matrix elem, state, (state, elem, Point -> state) -> state
walkWithIndex = \matrix, state, fn ->
    List.walkWithIndex matrix state \acc, row, y ->
        List.walkWithIndex row acc \accY, elem, x -> fn accY elem {x, y}

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

countIf : Matrix a, (a -> Bool) -> Nat
countIf = \matrix, predicate ->
    List.map matrix (\line -> List.countIf line predicate)
        |> List.sum

walkColumn : Matrix elem, Nat, state, (state, elem -> state) -> Result state [OutOfBounds]
walkColumn = \matrix, column, state, fn ->
    List.walk matrix (Ok state) \accResult, row ->
        elem <- List.get row column |> Result.try
        Result.map accResult \acc -> (fn acc elem)

transpose : Matrix elem -> Matrix elem
transpose = \matrix ->
    List.get matrix 0
        |> Result.withDefault []
        |> List.walkWithIndex [] \newMatrix, _, x ->
            when getColumn matrix x is
                Ok column -> List.append newMatrix column
                Err _ -> newMatrix

# Inlined from ResultExtra
resultMap2 : (a, b -> c), Result a err, Result b err -> Result c err
resultMap2 = \fn, resA, resB ->
    a <- Result.try resA
    b <- Result.map resB
    fn a b

# Inlined from ResultExtra
resultSequence : List (Result a b) -> Result (List a) b
resultSequence = \list ->
    List.walk list (Ok []) (\acc, elem -> resultMap2 List.append acc elem)

turnRight : Matrix elem -> Matrix elem
turnRight = \matrix ->
    length = len matrix
    newMatrix = List.repeat (List.repeat (Err Nothing) length.y) length.x
    filledMatrix = mapWithIndex newMatrix \_, {x, y} ->
        get matrix {x: y, y: length.x - x - 1}
    List.map filledMatrix resultSequence
        |> resultSequence
        |> Result.withDefault []


mapColumnsWithIndex : Matrix elem, (List elem, Nat -> List elem) -> Matrix elem
mapColumnsWithIndex = \matrix, fn ->
    List.get matrix 0
        |> Result.withDefault []
        |> List.walkWithIndex matrix \newMatrix, _, x ->
            when getColumn matrix x is
                Err _ -> newMatrix
                Ok column -> setColumn newMatrix x (fn column x)

walkColumnsWithIndex : Matrix elem, state, (state, List elem, Nat -> state) -> state
walkColumnsWithIndex = \matrix, state, fn ->
    List.get matrix 0
        |> Result.withDefault []
        |> List.walkWithIndex state \columnState, _, x ->
            when getColumn matrix x is
                Ok column -> fn columnState column x
                Err _ -> columnState

getColumn : Matrix elem, Nat -> Result (List elem) [OutOfBounds]
getColumn = \matrix, columnIndex ->
    walkColumn matrix columnIndex [] \column, elem -> List.append column elem

setColumn : Matrix elem, Nat, List elem -> Matrix elem
setColumn = \matrix, x, column ->
    List.walkWithIndex column matrix \newMatrix, elem, y ->
        set newMatrix {x, y} elem

allColumn : Matrix elem, Nat, (elem -> Bool) -> Bool
allColumn = \matrix, column, predicate ->
    walkColumn matrix column Bool.false (\acc, elem -> acc && predicate elem)
        |> Result.withDefault Bool.false

anyColumn : Matrix elem, Nat, (elem -> Bool) -> Bool
anyColumn = \matrix, column, predicate ->
    walkColumn matrix column Bool.false (\acc, elem -> acc || predicate elem)
        |> Result.withDefault Bool.false