app "hello"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [cli.Stdout, "input.txt" as input : Str]
    provides [main] to cli

Spring : [Damaged, Operational, Unknown]

strToSpring = \text ->
    when text is
        "." -> Operational
        "#" -> Damaged
        "?" -> Unknown
        _ -> Unknown

parseSprings = \text ->
    Str.graphemes text |> List.map strToSpring

parseGroups = \text ->
    Str.split text "," |> List.keepOks Str.toNat

parseLine : Str -> Result (List Spring, List Nat) [BadInput]
parseLine = \text ->
    when Str.split text " " is
        [springs, groups] -> Ok
            (
                parseSprings springs,
                parseGroups groups,
            )
        _ -> Err BadInput

isUnknown = \spring -> spring == Unknown

possibleArrangements : List Spring -> List (List Spring)
possibleArrangements = \springs ->
    when List.findFirstIndex springs isUnknown is
        Err NotFound -> [springs]
        Ok firstUnknown ->
            whatIfDamaged = List.set springs firstUnknown Damaged
                |> possibleArrangements
            whatIfOperational = List.set springs firstUnknown Operational
                |> possibleArrangements
            List.concat whatIfDamaged whatIfOperational

countingDamaged : (Nat, List Nat), Spring -> (Nat, List Nat)
countingDamaged = \(currentGroup, groups), spring ->
    when (currentGroup, spring) is
        (0, Operational) -> (0, groups)
        (newGroup, Operational) -> (0, List.append groups newGroup)
        (newGroup, Damaged) -> (newGroup + 1, groups)
        (newGroup, _) -> (newGroup, groups) # Impossible

countDamagedGroups : List Spring -> List Nat
countDamagedGroups = \springs ->
    (lastGroup, rest) = List.walk springs (0, []) countingDamaged
    if (lastGroup != 0)
        then List.append rest lastGroup
        else rest

expect countDamagedGroups (parseSprings ".#..###.##...") == [1,3,2]

isValidArrangement : List Nat -> (List Spring -> Bool)
isValidArrangement = \damagedGroups -> \springs ->
    countDamagedGroups springs == damagedGroups

countValidArrangements : (List Spring, List Nat) -> Nat
countValidArrangements = \(springs, damagedGroups) ->
    possibleArrangements springs
        |> List.countIf (isValidArrangement damagedGroups)

main =
    lines = Str.split input "\n"
    springLists = List.keepOks lines parseLine
    validArrangements = List.map springLists countValidArrangements
    # dbg validArrangements
    Stdout.line (List.sum validArrangements |> Num.toStr)