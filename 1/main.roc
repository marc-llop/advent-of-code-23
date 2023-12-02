app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "test.txt" as input : Str]
    provides [main] to cli

main =
    lines = Str.split input "\n"
    List.keepOks lines lineToCalibration
        |> List.sum
        |> Num.toStr
        |> Stdout.line

lineToCalibration : Str -> Result Nat [ListWasEmpty]
lineToCalibration = \line ->
    numbers : List Nat
    numbers = Str.graphemes line
        |> List.keepOks Str.toNat
    
    firstNum <- List.first numbers |> Result.try
    lastNum <- List.last numbers |> Result.try
    Ok (firstNum * 10 + lastNum)
