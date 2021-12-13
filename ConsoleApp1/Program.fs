// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let numbers = System.IO.File.ReadLines(fileName) |> Seq.cast<string> |> Seq.map int

// part one

let offsetSeq = fun xs skip truncateBy ->
    let len = Seq.length xs
    Seq.truncate (len - truncateBy) xs |> Seq.skip skip

let countIncreases = fun xs -> 
    let prevNumbers = offsetSeq xs 0 1
    let nextNumbers = offsetSeq xs 1 0
    let numIncreases = Seq.zip prevNumbers nextNumbers |> Seq.where ( fun (p,n) -> n > p ) |> Seq.length
    numIncreases

printfn "%i" (countIncreases numbers)

// part two

let threeWindowSums = fun xs ->
    let windowSize = 3
    let offsetSeqs =
        seq { 0 .. windowSize }
            |> Seq.map (fun offset -> offsetSeq xs offset (windowSize - offset - 1)) |> Seq.toArray
    let sums = Seq.zip3 offsetSeqs[0] offsetSeqs[1] offsetSeqs[2] |> Seq.map (fun (a,b,c) -> a+b+c)
    printfn "%A" sums
    sums

printfn "%i" (countIncreases (threeWindowSums numbers))
