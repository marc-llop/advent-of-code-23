app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str]
    provides [main] to cli

Step : List Nat

parseInput : Str -> List Step
parseInput = \text ->
    Str.replaceEach text "\n" ""
        |> Str.split ","
        |> List.map \step ->
            Str.toScalars step |> List.map Num.toNat

hash : Step -> Nat
hash = \step ->
    List.walk step 0 \currentValue, character ->
        currentValue
            |> \v -> v + character
            |> \v -> v * 17
            |> \v -> v % 256

main =
    hashes = parseInput input
        |> List.map hash
    hashSum = List.sum hashes
    Stdout.line (Num.toStr hashSum) 