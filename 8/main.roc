app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "./roc-parser/package/main.roc"
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, const, skip, map, keep, oneOrMore, flatten, sepBy },
        parser.String.{ parseStr, string, oneOf, anyCodeunit, codeunit, anyString },
        "input.txt" as input : Str,
    ]
    provides [main] to cli

Node : Str

startNode : Node
startNode = "AAA"

finalNode : Node
finalNode = "ZZZ"

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
        Ok (newSteps, nextNode) -> if nextNode == finalNode
            then newSteps
            else stepsToDestination desertMap (newSteps, nextNode)
        Err e -> 
            dbg e
            stepsToDestination desertMap (steps + 1, currentNode)
    
countSteps : Map -> Nat
countSteps = \desertMap ->
    stepsToDestination desertMap (0, startNode)

main =
    steps = when parseMap input is
        Ok m -> countSteps m |> Num.toStr
        Err (ParsingFailure e) -> "ParsingFailure \(e)"
        Err (ParsingIncomplete e) -> "ParsingIncomplete \(e)"
    Stdout.line steps