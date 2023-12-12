app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str, Matrix.{Matrix, Point}]
    provides [main] to cli

Pipe : [Vertical, Horizontal, TopLeft, TopRight, BottomLeft, BottomRight, Start, Nothing]

ContainmentStateMachine : [Outside, Inside, BottomCurve, TopCurve]

## Counts tiles inside the loop in a horizontal search by using a state machine that 
## follows the following patterns:
##  I | O
##  O | I
##
##  I ┌---┘ O  BottomCurve
##  I ┌---┐ I  BottomCurve
##  I └---┐ O  TopCurve
##  I └---┘ I  TopCurve
##
##  O ┌---┘ I  TopCurve
##  O ┌---┐ O  TopCurve
##  O └---┐ I  BottomCurve
##  O └---┘ O  BottomCurve
##
## Note this only works with a precleaned matrix: Pipes that do not belong to the loop should
## have been replaced with Nothing ('.'), and starting pipe ('S') has been replaced with their
## appropriate type.
countContainedTiles : (Nat, ContainmentStateMachine), Pipe -> (Nat, ContainmentStateMachine)
countContainedTiles = \(containedTiles, state), pipe ->
    newContainedTiles = if state == Inside && pipe == Nothing
        then containedTiles + 1
        else containedTiles
    newState = when state is
        Inside -> when pipe is
            Vertical -> Outside
            BottomRight -> BottomCurve
            TopRight -> TopCurve
            _ -> Inside
        Outside -> when pipe is
            Vertical -> Inside
            BottomRight -> TopCurve
            TopRight -> BottomCurve
            _ -> Outside
        BottomCurve -> when pipe is
            BottomLeft -> Inside
            TopLeft -> Outside
            _ -> BottomCurve
        TopCurve -> when pipe is
            BottomLeft -> Outside
            TopLeft -> Inside
            _ -> TopCurve
    (newContainedTiles, newState)

containedTilesInLine : Nat, List Pipe -> Nat
containedTilesInLine = \containedTiles, line ->
    (newContainedTiles, _) = List.walk line (containedTiles, Outside) countContainedTiles
    newContainedTiles

expect containedTilesInLine 0 (lineToPipeList "...F----J..") == 2
expect containedTilesInLine 0 (lineToPipeList "...F----7..") == 0
expect containedTilesInLine 0 (lineToPipeList "...L--J....") == 0
expect containedTilesInLine 0 (lineToPipeList "...|....|..") == 4
expect containedTilesInLine 0 (lineToPipeList "....||.....") == 0
expect containedTilesInLine 0 (lineToPipeList ".FJ.||..L7.") == 3

containedTilesInMatrix : Matrix Pipe -> Nat
containedTilesInMatrix = \matrix ->
    List.walk matrix 0 containedTilesInLine

graphemeToPipe : Str -> Pipe
graphemeToPipe = \text ->
    when text is
        "-" -> Horizontal
        "|" -> Vertical
        "J" -> TopLeft
        "L" -> TopRight
        "7" -> BottomLeft
        "F" -> BottomRight
        "S" -> Start
        _ -> Nothing

pipeToStr : Pipe -> Str
pipeToStr = \pipe ->
    when pipe is
        Horizontal -> "-"
        Vertical -> "|"
        TopLeft -> "J"
        TopRight -> "L"
        BottomLeft -> "7"
        BottomRight -> "F"
        Start -> "S"
        Nothing -> "."

lineToPipeList : Str -> List Pipe
lineToPipeList = \text ->
    Str.graphemes text
        |> List.map graphemeToPipe

RelativePosition : [Top, Bottom, Left, Right]

Relative a : {position: RelativePosition, value: a}

MaybeRelativePoint : Result (Relative Point) [CulDeSac]

nextPoints : Matrix a, Point ->
    {
        goTop: MaybeRelativePoint,
        goBottom: MaybeRelativePoint,
        goLeft: MaybeRelativePoint,
        goRight: MaybeRelativePoint,
    }
nextPoints = \matrix, {x, y} ->
    matrixLength = Matrix.len matrix
    matrixLast = {x: matrixLength.x - 1, y: matrixLength.y - 1}
    goTop = if y > 0 
        then Ok {position: Top, value: {x, y: y - 1}}
        else Err CulDeSac
    goBottom = if y < matrixLast.y
        then Ok {position: Bottom, value: {x, y: y + 1}}
        else Err CulDeSac
    goLeft = if x > 0 
        then Ok {position: Left, value: {x: x - 1, y}}
        else Err CulDeSac
    goRight = if x < matrixLast.x 
        then Ok {position: Right, value: {x: x + 1, y}}
        else Err CulDeSac
    {goTop, goBottom, goLeft, goRight}

nextRelativePoint : Matrix Pipe, Relative Point, Pipe -> Result (Relative Point) [CulDeSac]
nextRelativePoint = \matrix, {position, value}, pipe ->
    {goTop, goBottom, goLeft, goRight} = nextPoints matrix value
    when (position, pipe) is
        (Top, Vertical) -> goTop
        (Top, BottomRight) -> goRight
        (Top, BottomLeft) -> goLeft
        (Bottom, Vertical) -> goBottom
        (Bottom, TopRight) -> goRight
        (Bottom, TopLeft) -> goLeft
        (Left, Horizontal) -> goLeft
        (Left, BottomRight) -> goBottom
        (Left, TopRight) -> goTop
        (Right, Horizontal) -> goRight
        (Right, BottomLeft) -> goBottom
        (Right, TopLeft) -> goTop
        _ -> Err CulDeSac

nextPipe : Matrix Pipe, Relative Point -> Result (Relative Point) [CulDeSac]
nextPipe = \matrix, current ->
    Matrix.get matrix current.value
        |> Result.mapErr (\_ -> CulDeSac)
        |> Result.try \currentPipe ->
            nextRelativePoint matrix current currentPipe
    
isViablePath : Matrix Pipe, Relative Point -> Bool
isViablePath = \matrix, relativePoint ->
    isViable = Matrix.get matrix relativePoint.value |> Result.try \pipe ->
        nextRelativePoint matrix relativePoint pipe
    Result.isOk isViable

findFirstPath : Matrix Pipe, Point -> Result (Relative Point) [NotFound]
findFirstPath = \matrix, startingPoint ->
    {goTop, goBottom, goRight, goLeft} = nextPoints matrix startingPoint
    List.keepOks [goTop, goBottom, goRight, goLeft] (\res -> res)
        |> List.findFirst \elem -> isViablePath matrix elem

isStart : Matrix Pipe, Relative Point -> Bool
isStart = \matrix, {value} ->
    Matrix.get matrix value
        |> Result.map \pipe -> pipe == Start
        |> Result.withDefault Bool.false

path : Matrix Pipe, List Point, Relative Point -> List Point
path = \matrix, visitedPoints, currentPoint ->
    if isStart matrix currentPoint
        then visitedPoints
        else when nextPipe matrix currentPoint is
            Ok point -> 
                newVisitedPoints = List.append visitedPoints point.value
                path matrix newVisitedPoints point
            Err _ -> visitedPoints

main =
    matrix = Str.split input "\n"
        |> List.map lineToPipeList

    maybePath : Result (List Point) [NotFound]
    maybePath = (
        startPosition <- Matrix.findFirstIndex matrix (\elem -> elem == Start) |> Result.try
        firstRelativePoint <- findFirstPath matrix startPosition |> Result.map
        pathPoints = path matrix [firstRelativePoint.value] firstRelativePoint
        pathPoints
    )
    
    pointsInPath = Result.withDefault maybePath []
    cleanMatrix = Matrix.mapWithIndex matrix \pipe, point -> 
        if List.contains pointsInPath point
            then if pipe == Start
                then BottomLeft # Hardcoded replacement for our specific input.
                else pipe
            else Nothing

    insideTiles = containedTilesInMatrix cleanMatrix

    # cleanStrMatrix = matrixMap cleanMatrix pipeToStr
    # Stdout.line (matrixToStr cleanStrMatrix)

    # farthestPoint = Result.withDefault maybeFarthestPoint 0
    Stdout.line (Num.toStr insideTiles)
