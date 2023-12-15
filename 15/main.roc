app "hello"
    packages { 
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "../packages/roc-parser/package/main.roc",
    }
    imports [cli.Stdout, cli.Task, "test.txt" as input : Str, parser.Core.{const, keep, skip, chompUntil, oneOf, map, flatten}, parser.String.{parseStr, digit, string}]
    provides [main] to cli

Label : Str

Lens : {label: Label, focalLength: Nat}

Step : [Insert Lens, Remove Label]

lensToStr = \{label, focalLength} ->
    "[\(label) \(Num.toStr focalLength)]"

stepToStr = \step ->
    when step is
        Insert {label, focalLength} -> "Insert \(label) \(Num.toStr focalLength)"
        Remove label -> "Remove \(label)"

parseStep : Str -> Result Step [ParsingError]
parseStep = \text ->
    toStringParser = \u8s ->
        when Str.fromUtf8 u8s is
            Ok str -> Ok str
            Err _ -> Err ""

    parser = oneOf [
        const (\label -> \focalLength -> Insert {label, focalLength})
            |> keep (chompUntil '=' |> map toStringParser |> flatten)
            |> skip (string "=")
            |> keep digit,
        const (\label -> Remove label)
            |> keep (chompUntil '-' |> map toStringParser |> flatten)
            |> skip (string "-")
    ]

    parseStr parser text
        |> Result.mapErr \_ -> ParsingError

parseInput : Str -> List Step
parseInput = \text ->
    Str.replaceEach text "\n" ""
        |> Str.split ","
        |> List.keepOks parseStep

hash : Step -> Nat
hash = \step ->
    stepLabel = when step is
        Insert {label} -> label
        Remove label -> label
    labelScalars = Str.toScalars stepLabel
        |> List.map Num.toNat
    List.walk labelScalars 0 \currentValue, character ->
        currentValue
            |> \v -> v + character
            |> \v -> v * 17
            |> \v -> v % 256


LensBox : (Dict Str Nat, List Lens)

newBox = (Dict.empty {}, [])

boxToStr : (Nat, LensBox) -> Str
boxToStr = \(label, (positionDict, lenses)) ->
    lensesStr = List.map lenses lensToStr
        |> Str.joinWith " "
    "Box \(Num.toStr label): \(lensesStr)"

insertLens : LensBox, Lens -> LensBox
insertLens = \(positionDict, lenses), lens ->
    when Dict.get positionDict lens.label is
        Ok index ->
            (positionDict, List.set lenses index lens)
        Err _ -> (
            Dict.insert positionDict lens.label (List.len lenses),
            List.append lenses lens,
        )



removeLens : LensBox, Label -> LensBox
removeLens = \(positionDict, lenses), labelToRemove ->
    when Dict.get positionDict labelToRemove is

        Ok index -> 
            newList = List.dropAt lenses index
            newDict = List.mapWithIndex newList (\{label}, i -> (label, i))
                |> Dict.fromList
            (newDict, newList)

        Err _ -> (positionDict, lenses)

performStep : Dict Nat LensBox, Step -> Dict Nat LensBox
performStep = \boxes, step ->
    operation = when step is
        Insert lens -> \box -> insertLens box lens
        Remove label -> \box -> removeLens box label

    Dict.update boxes (hash step) \maybeBox ->
        when maybeBox is
            Present box -> Present (operation box)
            Missing -> Present (operation newBox)

# initializationSequence : List Step -> List (Nat, LensBox)
initializationSequence = \steps ->
    boxes = Dict.empty {}
    List.walk steps boxes performStep
        |> Dict.toList

debug = \steps ->
    boxes = Dict.empty {}
    List.walk steps (Stdout.line "", boxes) \(task, dict), step ->
        newBoxes = performStep dict step
        (
            _ <- Task.await task
            _ <- Stdout.line (stepToStr step) |> Task.await
            _ <- Stdout.line (boxesToStr newBoxes) |> Task.await
            Stdout.line "\n",
            newBoxes
        )

boxesToStr : Dict Nat LensBox -> Str
boxesToStr = \boxes ->
    Dict.toList boxes
        |> List.map boxToStr
        |> Str.joinWith "\n"

main =
    steps = parseInput input
    (task, boxes) = debug steps
    task
    # box = insertLens newBox {label: "cs", focalLength: 7}
    #     |> insertLens {label: "cs", focalLength: 5}
    #     |> insertLens {label: "ab", focalLength: 2}
    # dbg box
    # Stdout.line "hi"