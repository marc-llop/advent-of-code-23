app "repro"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout]
    provides [main] to cli

identity = \a -> a

compose = \fnA, fnB ->
    \a -> fnB (fnA a)

chain = \fns ->
    List.walk fns identity compose

main =
    values = List.map [1,2,3] \x -> (chain [(\a -> a * 2), (\b -> b + 1)]) x
    Stdout.line "hi"