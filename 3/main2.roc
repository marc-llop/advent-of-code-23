app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, cli.Task, "input.txt" as input : Str]
    provides [main] to cli

Point : {x: I16, y: I16}

Number : {value: Nat, startPos: Point, endPos: Point}

Char : [Symbol Str, Star, Digit Nat, Nothing]

Matrix : List (List Char)

Engine : {matrix: Matrix, numbers: List Number, possibleGears: List Point}

concatDigits : List [Digit Nat] -> Nat
concatDigits = \digits ->
    List.walk digits 0 (\sum, Digit digit -> (sum * 10) + digit)

expect concatDigits [] == 0
expect concatDigits [Digit 9] == 9
expect concatDigits [Digit 1, Digit 2, Digit 3] == 123

buildNumber : List [Digit Nat], Point -> Number
buildNumber = \digits, endingPoint ->
    {
        value: concatDigits digits,
        startPos: {endingPoint & x: endingPoint.x - (Num.toI16 (List.len digits) - 1)},
        endPos: endingPoint,
    }

graphemeToChar : Str -> Char
graphemeToChar = \grapheme ->
    when Str.toNat grapheme is
        Ok nat -> Digit nat
        Err _ -> when grapheme is
            "." -> Nothing
            "*" -> Star
            _ -> Symbol grapheme

appendParsedNumberToNumbers : List Number, List [Digit Nat], Point -> List Number
appendParsedNumberToNumbers = \numbers, parsedNumber, firstPointAfterNumber ->
    newNum = buildNumber parsedNumber {firstPointAfterNumber & x: firstPointAfterNumber.x - 1}
    List.append numbers newNum

parseChar : {
    parsedChars: List Char,
    numbers: List Number,
    possibleGears: List Point,
    currentPoint: Point,
    parsedNumber: List [Digit Nat]
}, Str -> {
    parsedChars: List Char,
    numbers: List Number,
    possibleGears: List Point,
    currentPoint: Point,
    parsedNumber: List [Digit Nat]
}
parseChar = \{parsedChars, numbers, possibleGears, currentPoint, parsedNumber}, grapheme ->
    char = graphemeToChar grapheme
    nextPoint = { x: currentPoint.x + 1, y: currentPoint.y }
    nextParsedChars = List.append parsedChars char
    appendToParsedNumber = \digit -> List.append parsedNumber digit
    nextPossibleGears = if char == Star
        then List.append possibleGears currentPoint
        else possibleGears
    when char is
        Digit n -> {
            parsedChars: nextParsedChars,
            numbers,
            possibleGears,
            currentPoint: nextPoint,
            parsedNumber: appendToParsedNumber (Digit n)
        }
        _ -> if List.len parsedNumber > 0
            then {
                parsedChars: nextParsedChars,
                numbers: appendParsedNumberToNumbers numbers parsedNumber currentPoint,
                possibleGears: nextPossibleGears,
                currentPoint: nextPoint,
                parsedNumber: [],
            }
            else {
                parsedChars: nextParsedChars,
                numbers,
                possibleGears: nextPossibleGears,
                currentPoint: nextPoint,
                parsedNumber: [],
            }

# expect parseChar {
#         [Digit 3, Digit 5],
#         [],
#         {x:2, y:2},
#         [Digit 3, Digit 5]
#     ) "9" == (
#         [Digit 3, Digit 5, Digit 9],
#         [],
#         {x:3, y:2},
#         [Digit 3, Digit 5, Digit 9]
#     )

# expect parseChar (
#         [Digit 3, Digit 5],
#         [],
#         {x:2, y:2},
#         [Digit 3, Digit 5]
#     ) "." == (
#         [Digit 3, Digit 5, Nothing],
#         [{value: 35, startPos: {x: 0, y: 2}, endPos: {x: 2, y: 2}}],
#         {x: 3, y: 2},
#         []
#     )

parseLine : (List Number, List Point, Matrix, Point), Str -> (List Number, List Point, Matrix, Point)
parseLine = \(numbers, possibleGears, matrix, point), text ->
    graphemes = Str.graphemes text
    state = List.walk graphemes {parsedChars: [], numbers, possibleGears, currentPoint: point, parsedNumber: []} parseChar
    nextNumbers = state.numbers
    nextPossibleGears = state.possibleGears
    lastPointPlusOne = state.currentPoint
    unaddedDigits = state.parsedNumber

    finalNumbers = if List.len unaddedDigits > 0
        then appendParsedNumberToNumbers nextNumbers unaddedDigits lastPointPlusOne
        else nextNumbers
    (finalNumbers, nextPossibleGears, List.append matrix state.parsedChars, {x: 0, y: point.y + 1})

parseEngine : Str -> Engine
parseEngine = \text ->
    lines = Str.split text "\n"
    (numbers, possibleGears, matrix, _) = List.walk lines ([], [], [], {x: 0, y: 0}) parseLine
    {matrix, numbers, possibleGears}

intToNat : I16 -> Nat
intToNat = \i ->
    if i < 0
        then 0
        else Num.toNat i

matrixGet : Matrix, Point -> Result Char [OutOfBoundsY, OutOfBoundsX]
matrixGet = \matrix, {x, y} ->
    lres = List.get matrix (intToNat y)
    Result.try lres (\line ->
        List.get line (intToNat x) |> Result.mapErr \_ -> OutOfBoundsX)
        |> Result.mapErr \_ -> OutOfBoundsY

isSymbol : Char -> Bool
isSymbol = \char ->
    when char is
        Symbol _ -> Bool.true
        Star -> Bool.true
        _ -> Bool.false

cartesianProduct : List a, List b -> List (a, b)
cartesianProduct = \xs, ys ->
    List.joinMap xs \x ->
        List.map ys \y -> (x, y)

touchesPoint : Number, {x: Nat, y: Nat} -> Bool
touchesPoint = \{startPos, endPos}, point ->
    isStartX = startPos.x == 0
    isStartY = startPos.y == 0
    xStart = if isStartX then 0 else startPos.x - 1 |> intToNat
    yStart = if isStartY then 0 else startPos.y - 1 |> intToNat
    xLength = endPos.x - startPos.x + (if isStartX then 2 else 3) |> intToNat
    yLength = endPos.y - startPos.y + (if isStartY then 2 else 3) |> intToNat
    xRange = List.range {start: At xStart, end: Length xLength}
    yRange = List.range {start: At yStart, end: Length yLength}
    cartesianProduct xRange yRange
        |> List.map \(x, y) -> {x, y}
        |> List.contains point
    

numbersInPossibleGear : List Number, Point -> Result (Nat, Nat) [NotAGear]
numbersInPossibleGear = \allNumbers, point ->
    gearPoint = {x: point.x |> intToNat, y: point.y |> intToNat}
    numbersInGear = List.keepIf allNumbers \num -> touchesPoint num gearPoint
    when numbersInGear is
        [a, b] -> Ok (a.value, b.value)
        _ -> Err NotAGear

isPartOfEngine : Number, Matrix -> Bool
isPartOfEngine = \{startPos, endPos, value}, matrix ->
    isStartX = startPos.x == 0
    isStartY = startPos.y == 0
    xStart = if isStartX then 0 else startPos.x - 1 |> intToNat
    yStart = if isStartY then 0 else startPos.y - 1 |> intToNat
    xLength = endPos.x - startPos.x + (if isStartX then 2 else 3) |> intToNat
    yLength = endPos.y - startPos.y + (if isStartY then 2 else 3) |> intToNat

    submatrix = List.sublist matrix {start: yStart, len: yLength}
        |> List.map \line -> List.sublist line {start: xStart, len: xLength}
        
    matrixAny submatrix isSymbol

matrixMap : List (List a), (a -> b) -> List (List b)
matrixMap = \matrix, fn ->
    List.map matrix \line ->
        List.map line fn

matrixAny : List (List a), (a -> Bool) -> Bool
matrixAny = \matrix, fn ->
    List.any matrix \line ->
        List.any line fn

matrixAll : List (List a), (a -> Bool) -> Bool
matrixAll = \matrix, fn ->
    List.all matrix \line ->
        List.all line fn

printMatrix : List (List Str) -> Str
printMatrix = \matrix ->
    List.map matrix (\line -> Str.joinWith line " ")
        |> Str.joinWith "\n"

not : (a -> Bool) -> (a -> Bool)
not = \fn ->
    \a -> !(fn a)

checkBug : Number, Matrix -> Str
checkBug = \{startPos, endPos, value}, matrix ->
    isStartX = startPos.x == 0
    isStartY = startPos.y == 0
    xStart = if isStartX then 0 else startPos.x - 1 |> intToNat
    yStart = if isStartY then 0 else startPos.y - 1 |> intToNat
    xLength = endPos.x - startPos.x + (if isStartX then 2 else 3) |> intToNat
    yLength = endPos.y - startPos.y + (if isStartY then 2 else 3) |> intToNat
    submatrix = List.sublist matrix {start: yStart, len: yLength}
        |> List.map \line -> List.sublist line {start: xStart, len: xLength}
    submatrixC = matrixMap submatrix charToStr

    if (matrixAll submatrix (not isSymbol))
        then "\(printMatrix submatrixC)\n\(Num.toStr value)\n"
        else "\n"

charToStr : Char -> Str
charToStr = \c ->
    when c is
        Symbol x -> "\(x)"
        Star -> "*"
        Digit n -> "\(Num.toStr n)"
        Nothing -> "."

sequence : List (Task.Task {} b) -> Task.Task {} b
sequence = \tasks ->
    List.walk tasks (Task.ok {}) \fullTask, task -> 
        Task.await fullTask (\_ -> task)

main =
    {matrix, numbers, possibleGears} = parseEngine input
    gears = List.keepOks possibleGears \gear -> numbersInPossibleGear numbers gear
    sum = gears
        |> List.map (\(a, b) -> a * b)
        |> List.sum
        |> Num.toStr
    Stdout.line sum
    