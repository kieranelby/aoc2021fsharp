// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines = System.IO.File.ReadLines(fileName) |> Seq.cast<string>

let inputHeightmap =
    inputLines |> Seq.map (
        fun l -> l.Trim().ToCharArray() |> Seq.map (fun c -> int c - int '0') )
    |> array2D

let locations (heightmap : int[,]) =
    seq {
        for i0 in 0 .. heightmap.GetLength(0) - 1 do
            for i1 in 0 .. heightmap.GetLength(1) - 1 do
                yield (i0, i1)
    }

let adjacentLocations (heightmap : int[,]) location =
    seq {
        let i0, i1 = location
        if i0 > 0 then
            yield (i0 - 1, i1)
        if i0 < heightmap.GetLength(0) - 1 then
            yield (i0 + 1, i1)
        if i1 > 0 then
            yield (i0, i1 - 1)
        if i1 < heightmap.GetLength(1) - 1 then
            yield (i0, i1 + 1)
    }

let heightAt (heightmap : int[,]) location =
    let i0, i1 = location
    heightmap[i0, i1]

let isLowPoint (heightmap : int[,]) location =
    let height = heightAt heightmap location
    let adjacentHeights = adjacentLocations heightmap location |> Seq.map (heightAt heightmap)
    adjacentHeights |> Seq.forall (fun x -> height < x)

let riskLevel (heightmap : int[,]) location = 1 + heightAt heightmap location

let partOne heightmap = locations heightmap |> Seq.filter (isLowPoint heightmap) |> Seq.map (riskLevel heightmap) |> Seq.sum

printfn $"%A{partOne inputHeightmap}"

let rec floodBasin (heightmap : int[,]) wave visited =
    if Set.isEmpty wave then
        visited
    else
        let floodNextFrom location = adjacentLocations heightmap location |> Seq.filter (fun al ->
            let height = heightAt heightmap location
            let adjacentHeight = heightAt heightmap al
            (not (Set.contains al visited)) && adjacentHeight < 9 && adjacentHeight > height)
        let nextWave = wave |> Seq.collect floodNextFrom |> Set
        floodBasin heightmap nextWave (visited + wave)

let floodBasinFrom heightmap (lowPoint : int * int) =
    floodBasin heightmap (seq { lowPoint } |> Set) Set.empty

let partTwo heightmap =
    locations heightmap
        |> Seq.filter (isLowPoint heightmap)
        |> Seq.map (floodBasinFrom heightmap)
        |> Seq.map (Seq.length)
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.reduce (fun a b -> a * b)

printfn $"%A{partTwo inputHeightmap}"
