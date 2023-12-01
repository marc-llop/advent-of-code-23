app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "test.txt" as input : Str]
    provides [main] to cli

main =
    lines = Str.split input "\n"
    firstLine = List.get lines 0 |> Result.withDefault ""
    Stdout.line firstLine