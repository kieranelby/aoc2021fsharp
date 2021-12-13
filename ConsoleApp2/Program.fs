// Advent of Code 2021 - F# noob

type Command =
    | Forward of int
    | Down of int
    | Up of int

let parseCommand = fun (s:string) ->
    let parts = s.Split(' ')
    let commandStr = parts[0]
    let distanceStr = parts[1]
    let distance = int distanceStr
    match commandStr with
    | "forward" -> Forward distance
    | "down" -> Down distance
    | "up" -> Up distance
    | _ -> failwith "unknown command"

//let fileName = "example.txt"
let fileName = "input.txt"
let commands = System.IO.File.ReadLines(fileName) |> Seq.cast<string> |> Seq.map parseCommand

type Submarine = { HorizontalPosition : int; Depth : int ; Aim : int }

let moveSubPartOne = fun sub command ->
    match command with
    | Forward n -> { sub with HorizontalPosition = sub.HorizontalPosition + n }
    | Down n -> { sub with Depth = sub.Depth + n }
    | Up n -> { sub with Depth = sub.Depth - n }

let initialSub = { HorizontalPosition = 0 ; Depth = 0 ; Aim = 0 }

let finalSubOne = Seq.fold moveSubPartOne initialSub commands
printfn $"partOne = {finalSubOne.HorizontalPosition * finalSubOne.Depth} from {finalSubOne}"

let moveSubPartTwo = fun sub command ->
    match command with
    | Forward n -> { sub with HorizontalPosition = sub.HorizontalPosition + n ; Depth = sub.Depth + sub.Aim * n }
    | Down n -> { sub with Aim = sub.Aim + n }
    | Up n -> { sub with Aim = sub.Aim - n }

let finalSubTwo = Seq.fold moveSubPartTwo initialSub commands
printfn $"partTwo = {finalSubTwo.HorizontalPosition * finalSubTwo.Depth} from {finalSubTwo}"
