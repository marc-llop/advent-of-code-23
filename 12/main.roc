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

# possibleArrangements : List Spring -> List (List Spring)
# possibleArrangements = \springs ->
#     when List.findFirstIndex springs isUnknown is
#         Err NotFound -> [springs]
#         Ok firstUnknown ->
#             whatIfDamaged = List.set springs firstUnknown Damaged
#                 |> possibleArrangements
#             whatIfOperational = List.set springs firstUnknown Operational
#                 |> possibleArrangements
#             List.concat whatIfDamaged whatIfOperational

Sublist : {springs: List Spring, groups: Groups}

sublistToStr : Sublist -> Str
sublistToStr = \{springs, groups} ->
    springToStr = \s -> when s is
        Damaged -> "#"
        Operational -> "."
        _ -> "?"
    springsStr = List.map springs springToStr |> Str.joinWith ""
    groupsStr = groupsToList groups |> List.map Num.toStr |> Str.joinWith ","
    "\(springsStr) [\(groupsStr)]"

sublistAppend : Sublist, Spring -> Sublist
sublistAppend = \{springs, groups}, spring ->
    {
        springs: List.append springs spring,
        groups: updateGroups groups spring,
    }

emptySublist = {springs: [], groups: Closed []}

possibleArrangements : List Spring, List Nat, Sublist -> Nat
possibleArrangements = \originalList, damagedGroups, sublist ->
    index = List.len sublist.springs
    spring = List.get originalList index
    when spring is
        Err OutOfBounds -> 
            # dbg sublistToStr sublist
            # dbg areGroupsSublistOfDbg damagedGroups sublist.groups
            if isValidArrangement sublist damagedGroups 
                then 
                    # dbg sublistToStr sublist
                    1
                else 0
        Ok Damaged -> 
            newSublist = sublistAppend sublist Damaged
            possibleArrangements originalList damagedGroups newSublist
        Ok Operational ->
            newSublist = sublistAppend sublist Operational
            possibleArrangements originalList damagedGroups newSublist
        Ok Unknown ->
            whatIfDamaged = sublistAppend sublist Damaged
            # dbg (whatIfDamaged, isValidSublist whatIfDamaged damagedGroups)
            possibleArrangementsIfDamaged =
                if isValidSublist whatIfDamaged damagedGroups
                    then possibleArrangements originalList damagedGroups whatIfDamaged
                    else 0
            whatIfOperational = sublistAppend sublist Operational
            # dbg "----"
            # dbg (whatIfOperational, isValidSublist whatIfOperational damagedGroups)
            possibleArrangementsIfOperational =
                if isValidSublist whatIfOperational damagedGroups
                    then possibleArrangements originalList damagedGroups whatIfOperational
                    else 0
            (possibleArrangementsIfDamaged + possibleArrangementsIfOperational)

isValidSublist : Sublist, List Nat -> Bool
isValidSublist = \sublist, damagedGroups ->
    areGroupsSublistOf damagedGroups sublist.groups

isValidArrangement : Sublist, List Nat -> Bool
isValidArrangement = \{groups}, damagedGroups ->
    groupsToList groups == damagedGroups

Groups : [Open Nat (List Nat), Closed (List Nat)]

updateGroups : Groups, Spring -> Groups
updateGroups = \groups, spring ->
    when (groups, spring) is
        (Open group list, Damaged) -> Open (group + 1) list
        (Open group list, Operational) -> Closed (List.append list group)
        (Closed list, Damaged) -> Open 1 list
        (Closed list, Operational) -> Closed list
        (_, Unknown) -> groups # Not used

groupsToList : Groups -> List Nat
groupsToList = \groups ->
    when groups is
        Open group list -> List.append list group
        Closed list -> list

areGroupsSublistOfDbg : List Nat, Groups -> Bool
areGroupsSublistOfDbg = \list, subgroups ->
    possibleSublist = groupsToList subgroups
    sublist = List.takeFirst list (List.len possibleSublist)
    # dbg sublist
    sublist == possibleSublist

areGroupsSublistOf : List Nat, Groups -> Bool
areGroupsSublistOf = \list, subgroups ->
    when subgroups is
        Closed possibleSublist ->
            sublist = List.takeFirst list (List.len possibleSublist)
            sublist == possibleSublist
        Open group possibleSublist ->
            sublist = List.takeFirst list (List.len possibleSublist)
            nextGroup = List.get list (List.len possibleSublist) |> Result.withDefault 0
            sublist == possibleSublist && group <= nextGroup


countDamagedGroups : List Spring -> List Nat
countDamagedGroups = \springs ->
    groups = List.walk springs (Closed []) updateGroups
    groupsToList groups

expect countDamagedGroups (parseSprings ".#..###.##...") == [1,3,2]

# isValidArrangement : List Nat -> (List Spring -> Bool)
# isValidArrangement = \damagedGroups -> \springs ->
#     countDamagedGroups springs == damagedGroups

countValidArrangements : (List Spring, List Nat) -> Nat
countValidArrangements = \(springs, damagedGroups) ->
    possibleArrangements springs damagedGroups emptySublist

unfoldList : (List Spring, List Nat) -> (List Spring, List Nat)
unfoldList = \(springs, damagedGroups) ->
    unfoldedSprings = List.repeat springs 5
        |> List.intersperse [Unknown]
        |> List.join
    unfoldedGroups = List.repeat damagedGroups 5
        |> List.join
    (unfoldedSprings, unfoldedGroups)

main =
    lines = Str.split input "\n"
    springLists = List.keepOks lines parseLine
        |> List.map unfoldList
    # dbg isValidSublist { groups: Closed [1], springs: [Damaged, Operational] } [1,1,3,1,1,3,1,1,3,1,1,3,1,1,3]
    validArrangements = List.map springLists \list -> 
        arrangements = countValidArrangements list
        dbg arrangements
        arrangements
    # dbg validArrangements
    Stdout.line (List.sum validArrangements |> Num.toStr)
    # Stdout.line "hi"