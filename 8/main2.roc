app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "./roc-parser/package/main.roc"
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, const, skip, map, keep, oneOrMore, flatten, sepBy },
        parser.String.{ parseStr, string, oneOf, anyCodeunit, codeunit, anyString },
        ResultExtra.{ sequence },
        "input.txt" as input : Str,
    ]
    provides [main] to cli

Node : Str

isStartNode : Node -> Bool
isStartNode = \node ->
    when Str.graphemes node is
        [_, _, "A"] -> Bool.true
        _ -> Bool.false

isEndNode : Node -> Bool
isEndNode = \node ->
    when Str.graphemes node is
        [_, _, "Z"] -> Bool.true
        _ -> Bool.false

Map : {instructions: List [R, L], nodes: Dict Node (Node, Node)}

rightParser = const R |> skip (string "R")
leftParser = const L |> skip (string "L")

instructionParser : Parser _ [L, R]
instructionParser = oneOf [rightParser, leftParser]
instructionsParser : Parser _ (List [L, R])
instructionsParser = oneOrMore instructionParser

nodeNameParser : Parser _ Node
nodeNameParser = 
    const (\x -> \y -> \z -> Str.fromUtf8 [x, y, z] |> Result.mapErr \_ -> "Error parsing in Str.fromUtf8")
    |> keep anyCodeunit
    |> keep anyCodeunit
    |> keep anyCodeunit
    |> flatten

nodeParser : Parser _ (Node, (Node, Node))
nodeParser =
    const (\name -> \nodeL -> \nodeR -> (name, (nodeL, nodeR)))
    |> keep nodeNameParser
    |> skip (string " = (")
    |> keep nodeNameParser
    |> skip (string ", ")
    |> keep nodeNameParser
    |> skip (string ")")

nodesParser : Parser _ (Dict Node (Node, Node))
nodesParser =
    sepBy nodeParser (codeunit '\n')
    |> map (Dict.fromList)

desertMapParser : Parser _ Map
desertMapParser = 
    const (\instructions -> \nodes -> {instructions, nodes})
    |> keep instructionsParser
    |> skip (string "\n\n")
    |> keep nodesParser
    |> skip (anyString)

parseMap : Str -> Result Map [ParsingFailure Str, ParsingIncomplete Str]
parseMap = \text ->
    parseStr desertMapParser text

loopBetween : Nat, Nat, Nat -> Nat
loopBetween = \begin, end, num ->
    num % (end - begin)

followStep : Map, (Nat, Node) -> Result (Nat, Node) [KeyNotFound, OutOfBounds]
followStep = \{instructions, nodes}, (steps, currentNode) ->
    currentIndex = loopBetween 0 (List.len instructions) steps
    currentInstruction <- List.get instructions currentIndex |> Result.try
    (leftNode, rightNode) <- Dict.get nodes currentNode |> Result.map
    when currentInstruction is
        L -> (steps + 1, leftNode)
        R -> (steps + 1, rightNode)

stepsToDestination : Map, (Nat, Node) -> Nat
stepsToDestination = \desertMap, (steps, currentNode) ->
    nextStep = followStep desertMap (steps, currentNode)
    when nextStep is
        Ok (newSteps, nextNode) -> if isEndNode nextNode
            then newSteps
            else stepsToDestination desertMap (newSteps, nextNode)
        Err e -> 
            dbg e
            stepsToDestination desertMap (steps + 1, currentNode)

gdc : Nat, Nat -> Nat
gdc = \x, y ->
    if x == y then x
    else if x > y then gdc (x - y) y
    else gdc x (y - x)

lcm : Nat, Nat -> Nat
lcm = \x, y ->
    (x * y) // (gdc x y)

lcmFold : List Nat -> Nat
lcmFold = \list ->
    List.walk list 1 lcm

countSteps : Map -> Nat
countSteps = \desertMap ->
    startNodes = Dict.keys desertMap.nodes
        |> List.keepIf isStartNode
    routesSteps = List.map startNodes \startNode -> stepsToDestination desertMap (0, startNode)
    dbg routesSteps
    lcmFold routesSteps

main =
    steps = when parseMap input is
        Ok m -> countSteps m |> Num.toStr
        Err (ParsingFailure e) -> "ParsingFailure \(e)"
        Err (ParsingIncomplete e) -> "ParsingIncomplete \(e)"
    Stdout.line steps