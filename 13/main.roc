app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str, Matrix]
    provides [main] to cli


isReflectionFrom : List a, List a -> Bool where a implements Eq
isReflectionFrom = \a, b ->
    reflectionLength = Num.min (List.len a) (List.len b)
    real = List.takeLast a reflectionLength
    reflection = List.takeFirst b reflectionLength |> List.reverse
    real == reflection

expect isReflectionFrom [1,2,3] [3,2,1,2] == Bool.true
expect isReflectionFrom [1,2] [3,3,2,1,2] == Bool.false
expect isReflectionFrom [1,2,3,3,2] [1,2] == Bool.false
expect isReflectionFrom [1,0,1,2,3] [3,2] == Bool.true
expect isReflectionFrom [1,0,1,2,3] [3,1] == Bool.false


findReflectionRec : List a, List a -> Result Nat [NotFound] where a implements Eq
findReflectionRec = \a, b ->
    if List.len b == 0 then Err NotFound
    else if isReflectionFrom a b
        then Ok (List.len a)
        else 
            {before, others} = List.split b 1
            findReflectionRec (List.concat a before) others

findReflection : List a -> Result Nat [NotFound] where a implements Eq
findReflection = \list ->
    when list is
        [_] -> Err NotFound
        [first, ..] -> findReflectionRec [first] (List.dropFirst list 1)
        _ -> Err NotFound

expect findReflection [1,2,3,3,2,1,2] == Ok 3
expect findReflection [1,0,1,2,3,3,2] == Ok 5
expect findReflection [1,0,1,2,3,3] == Ok 5
expect findReflection [2,2] == Ok 1
expect findReflection [2,4,4,2] == Ok 2

findVerticalSymmetry = \matrix ->
    Matrix.transpose matrix |> findReflection

findHorizontalSymmetry = \matrix ->
    findReflection matrix

findSymmetryIndex = \matrix ->
    when findHorizontalSymmetry matrix is
        Ok x -> x * 100
        Err _ -> when findVerticalSymmetry matrix is
            Ok y -> y
            Err _ -> 0

parseMatrix = \text -> Str.split text "\n" |> List.map Str.graphemes

main =
    matrixes = Str.split input "\n\n"
    symmetryIndexes = List.map matrixes parseMatrix
        |> List.map findSymmetryIndex
    dbg symmetryIndexes
    Stdout.line (Num.toStr (List.sum symmetryIndexes))