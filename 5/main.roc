app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "test.txt" as input : Str]
    provides [main] to cli

identity : a -> a
identity = \a -> a

log : a -> a
log = \a ->
    dbg a
    a

cond : (a -> Bool), (a -> b), (a -> b) -> (a -> b)
cond = \predicate, thenFn, elseFn ->
    \a ->
        if predicate a
            then thenFn a
            else elseFn a


# ----- APP CODE -----

Range : (Nat, Nat, Nat)

Mapping : Nat -> Nat

isInRange : Range -> (Nat -> Bool)
isInRange = \(_, sourceRangeStart, rangeLength) ->
    \ object ->
        object >= sourceRangeStart && object < (sourceRangeStart + rangeLength)
    
safeToNat : Int * -> Nat
safeToNat = \int ->
    Num.max 0 int
        |> Num.toNat

map : Range -> Mapping
map = \(destinationRangeStart, sourceRangeStart, _) ->
    iDestinationRangeStart = Num.toI16 destinationRangeStart
    iSourceRangeStart = Num.toI16 sourceRangeStart
    difference = iDestinationRangeStart - iSourceRangeStart

    \object ->
        (Num.toI16 object) + difference
            |> safeToNat

rangeMapping : Mapping, Range -> Mapping
rangeMapping = \elseMap, range ->
    thenMap = map range
    pred = isInRange range
    cond pred thenMap elseMap

categoryMapping : List Range -> Mapping
categoryMapping = \ranges ->
    List.walk ranges identity rangeMapping

exampleCategory = [
    (50, 98, 2),
    (52, 50, 48),
]

expect (categoryMapping exampleCategory) 79 == 81
expect (categoryMapping exampleCategory) 14 == 14
expect (categoryMapping exampleCategory) 55 == 57
expect (categoryMapping exampleCategory) 13 == 13
expect (categoryMapping exampleCategory) 99 == 51


main =
    (categoryMapping exampleCategory) 98
        |> Num.toStr
        |> Stdout.line
    # lines = Str.split input "\n"
    # firstLine = List.get lines 0 |> Result.withDefault ""
    # Stdout.line firstLine