// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputEdges =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
            |> Seq.map (fun l -> l.Trim().Split('-'))
            |> Seq.map (fun a -> (a[0], a[1]))
            |> List.ofSeq

let neighbouringCavesMap =
    inputEdges
    |> Seq.collect (fun (a,b) ->
        seq { (a, b) ; (b, a) })
    |> Seq.filter (fun (a, b) -> not (a = "end" || b = "start"))
    |> Seq.groupBy fst
    |> Seq.map (fun (cave, caves) -> (cave, caves |> Seq.map snd |> List.ofSeq))
    |> Map.ofSeq

let neighbouringCavesOf cave =
    neighbouringCavesMap
        |> Map.tryFind cave
        |> fun r ->
            match r with
            | Some caves -> caves
            | None -> List.empty

let samePath path1 path2 =
    0 = (List.compareWith (fun (a: string) b -> a.CompareTo b) path1 path2)

let isLegal path =
    path
        |> List.filter (fun (cave : string) -> cave.ToLower() = cave)
        |> List.countBy id
        |> List.countBy snd
        |> List.forall (fun (count, countOfTheCount) ->
            match count with
            | 1 -> true
            | 2 -> countOfTheCount <= 1
            | _ -> false )

let makeNewPaths path =
    let cave = List.head path
    cave
        |> neighbouringCavesOf
        |> List.map (fun next -> next :: path )
        |> List.filter isLegal
        |> List.ofSeq

let toPathString path = path |> String.concat ","
   
let rec searchPartOne completePaths partialPaths =
    if List.isEmpty partialPaths then
        completePaths
    else
        let newPaths =
            partialPaths
            |> List.collect makeNewPaths
        printfn $"""searching ... newPaths=%A{newPaths |> List.length}"""
        let completePathStrings =
            completePaths
                |> Seq.map toPathString
                |> Set.ofSeq
        let newCompletePaths =
            newPaths
            |> List.filter (
                fun p ->
                    "end" = (List.head p)
                    && not (
                        let ps = toPathString p
                        Set.contains ps completePathStrings )
            )
        let partialPathStrings =
            partialPaths
                |> Seq.map toPathString
                |> Set.ofSeq
        let newPartialPaths =
            newPaths
            |> List.filter (
                fun p ->
                    "end" <> (List.head p)
                    && not (
                        let ps = toPathString p
                        Set.contains ps partialPathStrings )
            )
        searchPartOne (List.append completePaths newCompletePaths) newPartialPaths

let partOne = searchPartOne [] [[ "start" ]] |> Seq.length

printfn $"""partOne=%A{partOne}"""
