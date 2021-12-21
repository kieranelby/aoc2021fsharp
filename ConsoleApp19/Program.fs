// Advent of Code 2021 - F# noob

open System.Text.RegularExpressions

//let fileName = "example-2d.txt"
//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
        |> Seq.map (fun l -> l.Trim())
        |> List.ofSeq

type UnknownScanner = { Number: int ; ReportedBeaconOffsets : List<int * int * int> }
type KnownScanner = { Number: int ; ReportedBeaconOffsets : List<int * int * int> ; Orientation : int[,] ; Position : int * int * int }
        
let rec parseInputR acc state lines =
    match state with
    | None ->
        match lines with
        | [] -> failwith "expected beacon lines right up until end of input"
        | head :: tail ->
            let m = Regex.Match(head, "--- scanner ([0-9]+) ---")
            if not m.Success then
                failwith $"expected scanner line here, not {head}"
            else
                let num = m.Groups[1].Value |> int
                let state' = Some (num, List.empty)
                parseInputR acc state' tail
    | Some currentScanner ->
        match lines with
        | [] -> currentScanner :: acc
        | head :: tail ->
            let m = Regex.Match(head, "(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)")
            if m.Success then
                let reportedBeaconOffset = (
                    m.Groups[1].Value |> int,
                    m.Groups[2].Value |> int,
                    m.Groups[3].Value |> int
                )
                let currentNum, currentReportedBeaconOffsets = currentScanner
                let state' = Some (currentNum, reportedBeaconOffset :: currentReportedBeaconOffsets)
                parseInputR acc state' tail
            elif head = "" then
                let acc' = currentScanner :: acc
                let state' = None
                parseInputR acc' state' tail
            else
                failwith $"expected beacon or blank line here, not {head}"

let parseInput lines =
    parseInputR List.empty None lines
    |> List.map (fun (scannerNum, reportedBeaconOffsets) ->
        { 
            Number = scannerNum ;
            ReportedBeaconOffsets = List.rev reportedBeaconOffsets
        } )
    |> List.rev
                
let input = parseInput inputLines

// source: http://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
let possibleOrientations =
    [
        array2D [ [ 1; 0; 0]
                  [ 0; 1; 0]
                  [ 0; 0; 1] ]
        array2D [ [ 0; 0; 1]
                  [ 0; 1; 0]
                  [-1; 0; 0] ]
        array2D [ [-1; 0; 0]
                  [ 0; 1; 0]
                  [ 0; 0;-1] ]
        array2D [ [ 0; 0;-1]
                  [ 0; 1; 0]
                  [ 1; 0; 0] ]

        array2D [ [ 0;-1; 0]
                  [ 1; 0; 0]
                  [ 0; 0; 1] ]
        array2D [ [ 0; 0; 1]
                  [ 1; 0; 0]
                  [ 0; 1; 0] ]
        array2D [ [ 0; 1; 0]
                  [ 1; 0; 0]
                  [ 0; 0;-1] ]
        array2D [ [ 0; 0;-1]
                  [ 1; 0; 0]
                  [ 0;-1; 0] ]

        array2D [ [ 0; 1; 0]
                  [-1; 0; 0]
                  [ 0; 0; 1] ]
        array2D [ [ 0; 0; 1]
                  [-1; 0; 0]
                  [ 0;-1; 0] ]
        array2D [ [ 0;-1; 0]
                  [-1; 0; 0]
                  [ 0; 0;-1] ]
        array2D [ [ 0; 0;-1]
                  [-1; 0; 0]
                  [ 0; 1; 0] ]

        array2D [ [ 1; 0; 0]
                  [ 0; 0;-1]
                  [ 0; 1; 0] ]
        array2D [ [ 0; 1; 0]
                  [ 0; 0;-1]
                  [-1; 0; 0] ]
        array2D [ [-1; 0; 0]
                  [ 0; 0;-1]
                  [ 0;-1; 0] ]
        array2D [ [ 0;-1; 0]
                  [ 0; 0;-1]
                  [ 1; 0; 0] ]

        array2D [ [ 1; 0; 0]
                  [ 0;-1; 0]
                  [ 0; 0;-1] ]
        array2D [ [ 0; 0;-1]
                  [ 0;-1; 0]
                  [-1; 0; 0] ]
        array2D [ [-1; 0; 0]
                  [ 0;-1; 0]
                  [ 0; 0; 1] ]
        array2D [ [ 0; 0; 1]
                  [ 0;-1; 0]
                  [ 1; 0; 0] ]

        array2D [ [ 1; 0; 0]
                  [ 0; 0; 1]
                  [ 0;-1; 0] ]
        array2D [ [ 0;-1; 0]
                  [ 0; 0; 1]
                  [-1; 0; 0] ]
        array2D [ [-1; 0; 0]
                  [ 0; 0; 1]
                  [ 0; 1; 0] ]
        array2D [ [ 0; 1; 0]
                  [ 0; 0; 1]
                  [ 1; 0; 0] ]
    ]

let mul (m : int[,]) (x,y,z) =
    ( m[0,0] * x + m[0,1] * y + m[0,2] * z,
      m[1,0] * x + m[1,1] * y + m[1,2] * z,
      m[2,0] * x + m[2,1] * y + m[2,2] * z )

let tranpose (m : int[,]) =
    array2D [ [ m[0,0]; m[1,0]; m[2,0] ]
              [ m[0,1]; m[1,1]; m[2,1] ]
              [ m[0,2]; m[1,2]; m[2,2] ] ]

let invMul (m : int[,]) coord =
    let t = tranpose m
    mul t coord

let add (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)
let sub (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)
let manhattan (x0,y0,z0) (x1,y1,z1) = abs(x0-x1) + abs(y0-y1) + abs (z0-z1)

let computeAbsoluteBeaconPositions knownScanner =
    knownScanner.ReportedBeaconOffsets
        |> List.map (mul knownScanner.Orientation)
        |> List.map (add knownScanner.Position)

let computeReportedBeaconOffsets absoluteBeaconPositions otherOrientation otherPosition =
    absoluteBeaconPositions
        |> List.map (sub otherPosition)
        |> List.map (invMul otherOrientation)

let tryMakeKnownOne (knownScanner : KnownScanner) (unknownScanner : UnknownScanner) =
    let knownAbsolutePositions = computeAbsoluteBeaconPositions knownScanner
    seq {
        for orientation in possibleOrientations do
            // suppose the scanner is in this orientation, but at the origin
            // what absolute positions would its reported offsets correspond to?
            let unknownUntranslatedAbsolutePositions =
                unknownScanner.ReportedBeaconOffsets
                    |> List.map (mul orientation)
            // compute (known position - unknown position) for every possible pair of beacons
            // hopefully there will be one vector that appears many times - that will give the
            // position of our unknown scanner
            let vectors =
                List.allPairs unknownUntranslatedAbsolutePositions knownAbsolutePositions
                |> List.map (
                    fun (uuap, kap) -> sub kap uuap)
            let mostCommonVector, count =
                vectors
                |> List.countBy id
                |> List.sortByDescending snd
                |> List.head
            let minCountNeeded = 12
            if count >= minCountNeeded then
                let newKnownScanner =
                    {  Number = unknownScanner.Number ;
                       ReportedBeaconOffsets = unknownScanner.ReportedBeaconOffsets ;
                       Orientation = orientation ;
                       Position = mostCommonVector ; }
                yield newKnownScanner
            else
                ()
    } |> Seq.tryHead

let tryMakeKnown (knownScanners : list<KnownScanner>) (unknownScanner : UnknownScanner) =
    knownScanners
        |> List.choose (fun k -> tryMakeKnownOne k unknownScanner)
        |> List.tryHead

let rec solvePartOneR knownScanners (unknownScanners : list<UnknownScanner>) =
    if List.isEmpty unknownScanners then
        knownScanners
    else
        let newKnownScanners =
            unknownScanners
                |> List.choose (tryMakeKnown knownScanners)
        if List.isEmpty newKnownScanners then
            failwith "could not figure out some scanners"
        else
            let knownScanners' = List.append knownScanners newKnownScanners
            let unknownScanners' =
                unknownScanners
                    |> List.filter (fun u -> not (List.exists (fun nk -> nk.Number = u.Number) newKnownScanners))
            solvePartOneR knownScanners' unknownScanners'

let locateScanners (unknownScanners : list<UnknownScanner>) =
    match unknownScanners with
    | [] -> failwith "need at least one scanner"
    | head :: tail ->
        let firstKnownScanner =
            {
                Number = head.Number ;
                ReportedBeaconOffsets = head.ReportedBeaconOffsets ;
                Orientation = possibleOrientations[0] ;
                Position = (0 , 0 , 0) ;
            }
        solvePartOneR [ firstKnownScanner ] tail

let knownScanners = locateScanners input

let partOne =
    knownScanners
        |> Seq.collect computeAbsoluteBeaconPositions
        |> Seq.distinct

let partTwo =
    Seq.allPairs knownScanners knownScanners
    |> Seq.map (fun (a, b) -> manhattan a.Position b.Position)
    |> Seq.max

printfn $"partOne: %A{partOne} beacons"
printfn $"partTwo: %A{partTwo} distance"
