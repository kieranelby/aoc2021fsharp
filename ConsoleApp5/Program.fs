// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines = System.IO.File.ReadLines(fileName) |> Seq.cast<string> |> Seq.cache

let parsePoint (str : string) =
    str.Split(",") |> Array.map int

let parseLine (str : string) : int[][] =
    str.Split(" -> ") |> Array.map parsePoint

type LineCategory =
    | Horizontal
    | Vertical
    | Diagonal

let categoriseLine (line : int[][]) =
    if line[0][1] = line[1][1] then
        Horizontal
    elif line[0][0] = line[1][0] then
        Vertical
    else
        Diagonal
    
let rasteriseLine (line : int[][]) =
    let startX = line[0][0]
    let endX = line[1][0]
    let startY = line[0][1]
    let endY = line[1][1]
    let signX = System.Math.Sign(endX - startX)
    let signY = System.Math.Sign(endY - startY)
    let dist = max (abs (endX - startX)) (abs (endY - startY))
    seq { 0 .. dist } |> Seq.map (fun i -> (startX + signX * i, startY + signY * i))

let numPointsWhereAtLeastTwoFilteredLinesOverlap linePredicate =
    inputLines |> Seq.map parseLine |> Seq.filter linePredicate |> Seq.collect rasteriseLine
        |> Seq.countBy id |> Seq.filter (fun (point,count) -> count >= 2) |> Seq.length

let isDiagonal ln =
    match categoriseLine ln with
    | Diagonal -> false
    | _ -> true

printfn $"partOne={numPointsWhereAtLeastTwoFilteredLinesOverlap isDiagonal}"
printfn $"partTwo={numPointsWhereAtLeastTwoFilteredLinesOverlap (fun x -> true)}"
