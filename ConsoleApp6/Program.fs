// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines = System.IO.File.ReadLines(fileName) |> Seq.cast<string> |> Seq.cache

let inputNumbers = Seq.head inputLines |> fun s -> s.Split ',' |> Seq.map int

printfn $"%A{inputNumbers}"

let nextLanternFishStates lf = seq {
        if lf = 0 then
            yield 6
            yield 8
        else
            yield lf - 1
    }

let rec simulateLanternFish lfs daysLeft =
    if daysLeft = 0 then
        lfs
    else
        let nextLfs = Seq.collect nextLanternFishStates lfs
        simulateLanternFish nextLfs (daysLeft - 1)

printfn $"after 18 days: {simulateLanternFish inputNumbers 18 |> Seq.length}"
printfn $"after 80 days: {simulateLanternFish inputNumbers 80 |> Seq.length}"
//printfn $"after 256 days: {simulateLanternFish inputNumbers 256 |> Seq.length}"

let mergeSameAges lfps =
    lfps |> Seq.groupBy fst |> Seq.map (fun (age,agesAndCounts) -> (age, (agesAndCounts |> Seq.map snd |> Seq.sum)))

let initialFishPopulations
    = Seq.map (fun age -> (age, 1m)) inputNumbers
        |> mergeSameAges

let nextLfpsStates lpfs = seq {
    let (age, count) = lpfs
    if age = 0 then
        yield (6, count)
        yield (8, count)
    else
        yield (age - 1, count)
}

let nextLanternFishPopulations lpfs =
    lpfs |> Seq.collect nextLfpsStates |> mergeSameAges

let rec simulateLanternFishPopulations lfps daysLeft =
    if daysLeft = 0 then
        lfps
    else
        let nextLfps = nextLanternFishPopulations lfps
        simulateLanternFishPopulations nextLfps (daysLeft - 1)

printfn $"initially: %A{initialFishPopulations}"
printfn $"after 18 days: {simulateLanternFishPopulations initialFishPopulations 18 |> Seq.map snd |> Seq.sum}"
printfn $"after 80 days: {simulateLanternFishPopulations initialFishPopulations 80 |> Seq.map snd |> Seq.sum}"
printfn $"after 256 days: {simulateLanternFishPopulations initialFishPopulations 256 |> Seq.map snd |> Seq.sum}"
