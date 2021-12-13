// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputCharArrays =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
            |> Seq.map (fun l -> l.Trim().ToCharArray() |> Seq.ofArray)

let charToSyntaxScore c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith $"unexpected char '{c}'"

let charToAutoCompletePoints c =
    match c with
    | '(' -> 1
    | '[' -> 2
    | '{' -> 3
    | '<' -> 4
    | _ -> failwith $"unexpected char '{c}'"

let isOpening c =
    match c with
    | '(' -> true
    | '[' -> true
    | '{' -> true
    | '<' -> true
    | _ -> false

let isMatching c1 c2 =
    match (c1, c2) with
    | ('(', ')') -> true
    | ('[', ']') -> true
    | ('{', '}') -> true
    | ('<', '>') -> true
    | _ -> false

let rec syntaxErrorScoreR chars stack =
    if Seq.isEmpty chars then
        None
    else
        let current = Seq.head chars
        if isOpening current then
            syntaxErrorScoreR (Seq.tail chars) (Seq.append (seq { current }) stack)
        else
            let lastOpened = Seq.head stack
            if isMatching lastOpened current then
                syntaxErrorScoreR (Seq.tail chars) (Seq.tail stack)
            else
                Some (charToSyntaxScore current)

let syntaxErrorScore chars =
    syntaxErrorScoreR chars Seq.empty

let partOne =
    inputCharArrays
        |> Seq.map syntaxErrorScore
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.sum

printfn $"{partOne}"

let incompleteLines = 
    inputCharArrays
        |> Seq.filter (fun line -> line |> syntaxErrorScore |> Option.isNone)

let rec autoCompleteStackScore stack =
    if Seq.isEmpty stack then
        0m
    else
        let head = Seq.head stack
        let tail = Seq.tail stack
        (charToAutoCompletePoints head |> decimal) + 5m * (autoCompleteStackScore tail)

let rec autoCompleteLineScoreR chars stack =
    if Seq.isEmpty chars then
        stack |> Seq.rev |> autoCompleteStackScore
    else
        let current = Seq.head chars
        if isOpening current then
            autoCompleteLineScoreR (Seq.tail chars) (Seq.append (seq { current }) stack)
        else
            let lastOpened = Seq.head stack
            if isMatching lastOpened current then
                autoCompleteLineScoreR (Seq.tail chars) (Seq.tail stack)
            else
                failwith "i thought we had filtered these out"

let autoCompleteLineScore chars =
    autoCompleteLineScoreR chars Seq.empty

let partTwo =
    let sortedScores =
        incompleteLines
            |> Seq.map autoCompleteLineScore
            |> Seq.sort
    let numScores = sortedScores |> Seq.length
    printfn $"%A{sortedScores}"
    sortedScores |> Seq.skip (numScores / 2) |> Seq.head

printfn $"{partTwo}"
