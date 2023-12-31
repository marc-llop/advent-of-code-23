app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str, HigherOrder.{chain, identity, cond}]
    provides [main] to cli

MapCase : (Nat, Nat, Nat)

Mapping : Nat -> Nat

Range : (Nat, Nat)

example =
    """
    seeds: 40 20 90 14

    seed-to-soil map:
    50 98 2
    52 50 48
    """
# initial ranges: (40, 20), (90, 14)
# ranges after thresholds: (40, 10), (50, 10), (90, 8), (98, 2), (100, 4)
# expected result ranges: (40, 10), (52, 10), (92, 8), (50, 2), (100, 4)
# ranges after collapsing: (40, 10), (50, 12), (92, 12)

listToRange = \list ->
    when list is
        [a, b] -> Ok (a, b)
        _ -> Err [InvalidRange]

parseSeeds : Str -> Result (List Range) [OutOfBounds]
parseSeeds = \text ->
    seedNumbers <- Str.split text "seeds: "
        |> List.get 1
        |> Result.map
        
    Str.split seedNumbers " "
        |> List.keepOks Str.toNat
        |> List.chunksOf 2
        |> List.keepOks listToRange

parseMapCase : Str -> Result MapCase [InvalidNumStr]
parseMapCase = \text ->
    numbers = Str.split text " "
        |> List.keepOks Str.toNat
        when numbers is
            [a, b, c] -> Ok (a, b, c)
            _ -> Err InvalidNumStr

parseCategory : Str -> Result (List MapCase) [InvalidCategory]
parseCategory = \text ->
    categoryMapCases <- Str.split text "map:\n"
        |> List.get 1
        |> Result.mapErr \_ -> InvalidCategory
        |> Result.map
    
    Str.split categoryMapCases "\n"
        |> List.keepOks parseMapCase

parseAlmanac : Str -> Result (List Range, List (List MapCase)) [ListWasEmpty, OutOfBounds]
parseAlmanac = \text ->
    textParts = Str.split (Str.trim text) "\n\n"
    seedsText <- List.first textParts |> Result.try
    seedRanges <- parseSeeds seedsText |> Result.map
    categoriesTexts = List.dropFirst textParts 1
    categories = List.keepOks categoriesTexts parseCategory
    (seedRanges, categories)

isInRange : MapCase -> (Nat -> Bool)
isInRange = \(_, sourceRangeStart, rangeLength) ->
    \ object ->
        object >= sourceRangeStart && object < (sourceRangeStart + rangeLength)
    
safeToNat : Int * -> Nat
safeToNat = \int ->
    Num.max 0 int
        |> Num.toNat

map : MapCase -> Mapping
map = \(destinationRangeStart, sourceRangeStart, _) ->
    iDestinationRangeStart = Num.toI64 destinationRangeStart
    iSourceRangeStart = Num.toI64 sourceRangeStart
    difference = iDestinationRangeStart - iSourceRangeStart

    \object ->
        (Num.toI64 object) + difference
            |> safeToNat

composeMapping : Mapping, MapCase -> Mapping
composeMapping = \elseMap, mapCase ->
    thenMap = map mapCase
    pred = isInRange mapCase
    cond pred thenMap elseMap

categoryMapping : List MapCase -> Mapping
categoryMapping = \mapCases ->
    List.walk mapCases identity composeMapping

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
    result = 
        (seedRanges, categories) <- parseAlmanac input |> Result.map
        dbg seedRanges
        Ok (seedRanges, categories)
        # mappings = List.map categories categoryMapping
        # seedToLocation = chain mappings
        # locations = List.map seeds \seed -> seedToLocation seed
        # List.min locations
        #     |> Result.withDefault 0
        #     |> Num.toStr

    when result is
        Ok s -> Stdout.line "hi"
        Err _ -> Stdout.line "Error"