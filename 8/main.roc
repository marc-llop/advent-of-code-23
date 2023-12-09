app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "./roc-parser/package/main.roc"
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, const, skip, map, keep, oneOrMore, flatten, sepBy },
        parser.String.{ parseStr, string, oneOf, anyCodeunit, codeunit, anyString },
        "test.txt" as input : Str,
    ]
    provides [main] to cli

Node : Str

Map : {instructions: List [R, L], nodes: Dict Node (Node, Node)}

right = const R |> skip (string "R")
left = const L |> skip (string "L")

instruction : Parser _ [L, R]
instruction = oneOf [right, left]
instructions : Parser _ (List [L, R])
instructions = oneOrMore instruction

nodeName : Parser _ Node
nodeName = 
    const (\x -> \y -> \z -> Str.fromUtf8 [x, y, z] |> Result.mapErr \_ -> "Error parsing in Str.fromUtf8")
    |> keep anyCodeunit
    |> keep anyCodeunit
    |> keep anyCodeunit
    |> flatten

node : Parser _ (Node, (Node, Node))
node =
    const (\name -> \nodeL -> \nodeR -> (name, (nodeL, nodeR)))
    |> keep nodeName
    |> skip (string " = (")
    |> keep nodeName
    |> skip (string ", ")
    |> keep nodeName
    |> skip (string ")")

nodes : Parser _ (Dict Node (Node, Node))
nodes =
    sepBy node (codeunit '\n')
    |> map (Dict.fromList)

desertMapParser : Parser _ Map
desertMapParser = 
    const (\ins -> \ns -> {instructions: ins, nodes: ns})
    |> keep instructions
    |> skip (string "\n\n")
    |> keep nodes
    |> skip (anyString)

parseMap : Str -> Result Map [ParsingFailure Str, ParsingIncomplete Str]
parseMap = \text ->
    parseStr desertMapParser text

main =
    desertMap = when parseMap input is
        Ok m -> dbg m
            "OK!"
        Err (ParsingFailure e) -> "ParsingFailure \(e)"
        Err (ParsingIncomplete e) -> "ParsingIncomplete \(e)"
    Stdout.line desertMap