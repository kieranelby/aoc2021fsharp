// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines = System.IO.File.ReadLines(fileName) |> Seq.cast<string> |> Seq.cache

let inputNumbers = Seq.head inputLines |> fun s -> s.Split ',' |> Seq.map int

let computeScorePartOne crabs position =
    crabs |> Seq.map (fun crab -> abs (position - crab)) |> Seq.sum

let computeScorePartTwo crabs position =
    crabs |> Seq.map (fun crab -> 
        let dist = abs (position - crab)
        (dist * (dist + 1)) / 2
    ) |> Seq.sum

let maximiseScore scoreFn crabs =
    let minPosition = crabs |> Seq.min
    let maxPosition = crabs |> Seq.max
    let positionsAndScores =
        seq { minPosition .. maxPosition } |> Seq.map (fun position -> (position, scoreFn crabs position))
    positionsAndScores |> Seq.sortBy snd |> Seq.head

printfn $"%A{maximiseScore computeScorePartOne inputNumbers}"
printfn $"%A{maximiseScore computeScorePartTwo inputNumbers}"
