// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputGrid =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
            |> Seq.map (fun l -> l.Trim().ToCharArray() |> Seq.ofArray |> Seq.map (fun c -> int c - int '0'))

let initialCavernState =
    inputGrid
        |> Seq.mapi (
            fun (y : int) levelsRow ->
                levelsRow |> Seq.mapi (
                    fun (x : int) level -> ((x,y), (level, 0))))
        |> Seq.collect id
        |> Map.ofSeq
        |> fun m -> (m, 0)

let incrementEnergy cavernState =
    let (octopodeMap, totalFlashes) = cavernState
    let nextOctopodeMap =
        octopodeMap
            |> Map.map (
                fun _ octopus -> 
                    let (level, flashedAt) = octopus
                    (level+1, flashedAt)
            )
    (nextOctopodeMap, totalFlashes)

let adjacentPositionsOf position =
    let (x,y) = position
    seq {
        (x-1,y-1)
        (x  ,y-1)
        (x+1,y-1)
        (x-1,y  )
        // don't include ourselves!
        (x+1,y  )
        (x-1,y+1)
        (x  ,y+1)
        (x+1,y+1)
    }

let rec flashAboveNine iterationNumber cavernState =
    let (octopodeMap, totalFlashes) = cavernState
    let octopodeMapWithNewFlashes =
        octopodeMap
            |> Map.map (
                fun _ octopus -> 
                    let (level, flashedAt) = octopus
                    if level > 9 && flashedAt = 0 then
                        (level, iterationNumber)
                    else
                        octopus
            )
    let newFlashes =
        octopodeMapWithNewFlashes
            |> Map.filter (
                fun _ octopus ->
                    let (_, flashedAt) = octopus
                    flashedAt = iterationNumber
            ) |> Map.count
    if newFlashes = 0 then
        cavernState
    else
        let octopodeMapWithAdjacentIncrements =
            octopodeMapWithNewFlashes
                |> Map.map (
                    fun position octopus ->
                        let (level, flashedAt) = octopus
                        if flashedAt > 0 then
                            octopus
                        else
                            let adjacent = adjacentPositionsOf position
                            let numFlashesNear =
                                adjacent
                                    |> Seq.filter (
                                        fun otherPosition ->
                                            let maybeOtherOctopus = Map.tryFind otherPosition octopodeMapWithNewFlashes
                                            match maybeOtherOctopus with
                                            | Some otherOctopus ->
                                                    let (_, otherFlashedAt) = otherOctopus
                                                    otherFlashedAt = iterationNumber
                                            | None -> false
                                    ) |> Seq.length
                            (level + numFlashesNear, flashedAt)
                )
        flashAboveNine (iterationNumber + 1) (octopodeMapWithAdjacentIncrements, totalFlashes + newFlashes)

let resetFlashed cavernState =
    let (octopodeMap, totalFlashes) = cavernState
    let nextOctopodeMap =
        octopodeMap
            |> Map.map (
                fun _ octopus -> 
                    let (_, flashedAt) = octopus
                    if flashedAt > 0 then
                        (0, 0)
                    else
                        octopus
            )
    (nextOctopodeMap, totalFlashes)

let advanceOne cavernState =
    cavernState
        |> incrementEnergy
        |> flashAboveNine 1
        |> resetFlashed

let rec advanceN n cavernState =
    if n = 0 then
        cavernState
    else
        advanceN (n - 1) (advanceOne cavernState)

printfn $"%A{advanceN 100 initialCavernState}"

let rec advanceUntilSimultaneous n cavernState =
    let nextCavernState =
        cavernState |> advanceOne
    let (octopodeMap, _) = nextCavernState
    let allJustFlashed =
        octopodeMap |> Map.forall (
            fun _ octopus ->
                let (level, _) = octopus
                level = 0
        )
    if allJustFlashed then
        n + 1
    else
        advanceUntilSimultaneous (n+1) nextCavernState

printfn $"%A{advanceUntilSimultaneous 0 initialCavernState}"
