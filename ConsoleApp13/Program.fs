// Advent of Code 2021 - F# noob

open System.Text.RegularExpressions

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
        |> Seq.map (fun l -> l.Trim())

type XOrY =
    | AlongX
    | AlongY

type Instruction =
    | DotInstruction of x : int * y : int
    | FoldInstruction of xOrY : XOrY * c : int
    | SeparatorInstruction

let (|DotPattern|_|) input =
    let m = Regex.Match(input, "([0-9]+),([0-9]+)")
    if (m.Success) then
        let x = m.Groups.[1].Value |> int
        let y = m.Groups.[2].Value |> int
        let dotInstruction = DotInstruction(x = x, y = y)
        Some dotInstruction
    else
        None

let (|FoldPattern|_|) input =
    let m = Regex.Match(input, "fold along ([xy])=([0-9]+)")
    if (m.Success) then
        let xOrY = match m.Groups.[1].Value with
            | "x" -> AlongX
            | "y" -> AlongY
            | _ -> failwith "regexp should only have accepted x or y"
        let n = m.Groups.[2].Value |> int
        let foldInstruction = FoldInstruction(xOrY = xOrY, c = n)
        Some foldInstruction
    else
        None

let parseInputLine (line : string) =
    match line with
    | DotPattern instruction -> instruction
    | FoldPattern instruction -> instruction
    | _ -> SeparatorInstruction

let applyInstruction maxFolds page instruction =
    let (dots, numFolds) = page
    match instruction with
    | DotInstruction (x, y) -> dots |> Set.add (x, y) |> fun d -> (d, numFolds)
    | FoldInstruction (AlongX, atX) ->
        dots |> Set.map (
            fun (x, y) ->
                if numFolds < maxFolds then
                    let x' =
                        if x > atX then
                            atX - (x - atX)
                        else
                            x
                    (x', y)
                else
                    (x, y)
        ) |> fun d -> (d, numFolds + 1)
    | FoldInstruction (AlongY, atY) ->
        dots |> Set.map (
            fun (x, y) ->
                if numFolds < maxFolds then
                    let y' =
                        if y > atY then
                            atY - (y - atY)
                        else
                            y
                    (x, y')
                else
                    (x, y)
        ) |> fun d -> (d, numFolds + 1)
    | SeparatorInstruction -> page

let applyInstructionPartOne page instruction = applyInstruction 1 page instruction

let partOne =
    inputLines
        |> Seq.map parseInputLine
        |> Seq.fold applyInstructionPartOne (Set.empty<(int * int)>, 0)
        |> fst |> Set.count

printfn $"""partOne=%A{partOne}"""

let applyInstructionPartTwo page instruction = applyInstruction System.Int32.MaxValue page instruction

let renderCell dots y x =
    if Set.contains (x, y) dots then
        "#"
    else
        "."

let renderRow dots y =
    seq { 0 .. 78 }
        |> Seq.map (renderCell dots y)
        |> String.concat ""

let render dots =
    seq { 0 .. 24 }
        |> Seq.map (renderRow dots)
        |> String.concat "\n"

let partTwo =
    inputLines
        |> Seq.map parseInputLine
        |> Seq.fold applyInstructionPartTwo (Set.empty<(int * int)>, 0)
        |> fst
        |> render

printfn $"""{partTwo}"""
