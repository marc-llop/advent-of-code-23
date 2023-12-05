app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str]
    provides [main] to cli

identity : a -> a
identity = \a -> a

# log : a -> a
# log = \a ->
#     dbg a
#     a

cond : (a -> Bool), (a -> b), (a -> b) -> (a -> b)
cond = \predicate, thenFn, elseFn ->
    \a ->
        if predicate a
            then thenFn a
            else elseFn a

# apply : (a -> b), a -> b
# apply = \fn, a -> fn a

compose : (a -> b), (b -> c) -> (a -> c)
compose = \fnA, fnB ->
    \a -> fnB (fnA a)

chain : List (a -> a) -> (a -> a)
chain = \fns ->
    List.walk fns identity compose

# ----- APP CODE -----

Range : (Nat, Nat, Nat)

Mapping : Nat -> Nat

parseSeeds : Str -> Result (List Nat) [OutOfBounds]
parseSeeds = \text ->
    seedNumbers <- Str.split text "seeds: "
        |> List.get 1
        |> Result.map
        
    Str.split seedNumbers " "
        |> List.keepOks Str.toNat

parseRange : Str -> Result Range [InvalidNumStr]
parseRange = \text ->
    numbers = Str.split text " "
        |> List.keepOks Str.toNat
        when numbers is
            [a, b, c] -> Ok (a, b, c)
            _ -> Err InvalidNumStr

parseCategory : Str -> Result (List Range) [InvalidCategory]
parseCategory = \text ->
    categoryRanges <- Str.split text "map:\n"
        |> List.get 1
        |> Result.mapErr \_ -> InvalidCategory
        |> Result.map
    
    Str.split categoryRanges "\n"
        |> List.keepOks parseRange

parseAlmanac : Str -> Result (List Nat, List (List Range)) [ListWasEmpty, OutOfBounds]
parseAlmanac = \text ->
    textParts = Str.split (Str.trim text) "\n\n"
    seedsText <- List.first textParts |> Result.try
    seeds <- parseSeeds seedsText |> Result.map
    categoriesTexts = List.dropFirst textParts 1
    categories = List.keepOks categoriesTexts parseCategory
    (seeds, categories)

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
    iDestinationRangeStart = Num.toI64 destinationRangeStart
    iSourceRangeStart = Num.toI64 sourceRangeStart
    difference = iDestinationRangeStart - iSourceRangeStart

    \object ->
        (Num.toI64 object) + difference
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
    result = 
        (seeds, categories) <- parseAlmanac input |> Result.map
        mappings = List.map categories categoryMapping
        seedToLocation = chain mappings
        locations = List.map seeds \seed -> seedToLocation seed
        List.min locations
            |> Result.withDefault 0
            |> Num.toStr

    when result is
        Ok s -> Stdout.line s
        Err _ -> Stdout.line "Error"
    # lines = Str.split input "\n"
    # firstLine = List.get lines 0 |> Result.withDefault ""
    # Stdout.line firstLine