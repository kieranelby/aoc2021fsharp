// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines = System.IO.File.ReadLines(fileName) |> Seq.cast<string> |> Seq.cache

// why is this not built into F# ? or can i just not search well
let splitOnPredicateExclusive (items : seq<_>) pred =
    seq {
        let mutable subList = []
        let enumerator = items.GetEnumerator()
        while enumerator.MoveNext() do
            let item = enumerator.Current
            if pred item then
                if not subList.IsEmpty then
                    yield List.rev subList |> Seq.ofList
                    subList <- []
            else
                subList <- item :: subList
        if not subList.IsEmpty then
            yield List.rev subList |> Seq.ofList
    }

let inputLinesChunks = splitOnPredicateExclusive inputLines System.String.IsNullOrWhiteSpace

let bingoNumbers =
    let parseNumbers (line : string) = line.Split ',' |> Seq.map int
    inputLinesChunks |> Seq.head |> Seq.head |> parseNumbers |> Seq.toList

let drawnNumbersAt drawCount = bingoNumbers |> fun ns -> ns.GetSlice(Some 0, Some drawCount)

let readBoards =
    let parseNumbers (line : string) =
        // watch out for repeated spaces
        line.Split ' ' |> Seq.filter (fun s -> s.Length > 0) |> Seq.map int
    let boardsChunks = inputLinesChunks |> Seq.tail
    let parseBoard = Seq.map parseNumbers
    Seq.map parseBoard boardsChunks

let scoreBoard board (numbers : seq<int>) =
    let rows = board
    // yay, F#6
    let cols = Seq.transpose board
    let unmarked = Seq.filter (fun n -> not (Seq.contains n numbers))
    let allMarked = unmarked >> Seq.isEmpty
    let rowsAndCols = Seq.append rows cols
    if rowsAndCols |> Seq.filter allMarked |> Seq.isEmpty then
        None
    else
        Some ((Seq.last numbers) * (rows |> Seq.collect unmarked |> Seq.sum))

let firstWinsExamineBoardsAt bs (drawCount : int) =
    let numbersSoFar = drawnNumbersAt drawCount
    let scores = bs |> Seq.map (fun b -> scoreBoard b numbersSoFar) |> Seq.filter (fun s -> s.IsSome)
    if Seq.isEmpty scores then
        None, bs
    else
        Seq.head scores, Seq.empty

let lastWinsExamineBoardsAt bs (drawCount : int) =
    let numbersSoFar = drawnNumbersAt drawCount
    let computeScore b = scoreBoard b numbersSoFar
    // why no Seq.partition?
    let winners = bs |> Seq.filter (fun b -> b |> computeScore |> Option.isSome)
    let losers = bs |> Seq.filter (fun b -> b |> computeScore |> Option.isNone)
    if Seq.isEmpty losers then
        match Seq.length winners with
        | 1 -> Seq.head winners |> computeScore , Seq.empty
        | _ -> failwith "expected to find just one last winner and stop there"
    else
        // throw away the winners while we still have one or more losers
        None , losers
    
let rec findFinalScore examineBoardsAt boards drawCount =
    let winningScore, keepBoards = examineBoardsAt boards drawCount
    match winningScore with
    | Some x -> x
    | None -> findFinalScore examineBoardsAt keepBoards (drawCount + 1)

printfn $"%A{findFinalScore firstWinsExamineBoardsAt readBoards 1}"
printfn $"%A{findFinalScore lastWinsExamineBoardsAt readBoards 1}"
