// Advent of Code 2021 - F# noob

open System.Text.RegularExpressions

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
        |> Seq.map (fun l -> l.Trim())

let parseTargetArea line =
    let m = Regex.Match(line, "target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)")
    if not m.Success then
        failwith "could not understand input"
    else
        ( m.Groups.[1].Value |> int, 
          m.Groups.[2].Value |> int,
          m.Groups.[3].Value |> int,
          m.Groups.[4].Value |> int )

let targetArea = Seq.head inputLines |> parseTargetArea

let tx0, tx1, ty0, ty1 = targetArea

if tx0 > tx1 || ty0 > ty1 then failwith "assumed would be ordered" else ()

let computeY ivy s = s * ivy - (s * (s - 1)) / 2

let rec computeXR vx s x =
    if s = 0 || vx = 0 then
        x
    else
        let x' = x + vx
        let vx' = if vx > 0 then vx - 1 else vx + 1
        let s' = s - 1
        computeXR vx' s' x'
    
let computeX ivx s =
    computeXR ivx s 0

let rec hitsTarget xv yv x y maxY previouslyHit =
    //printfn $"xv {xv} yv {yv} x {x} y {y} maxY {maxY} previouslyHit {previouslyHit}"
    let outsideWithNoChance = 
        // if we're left or right of the target and moving horizontally in the wrong direction, we can't do it
        (x < tx0 && xv <= 0) || 
        (x > tx1 && xv >= 0) ||
        // once we're below the target and moving down, we can't do it - though if we're above the
        // top and moving up we still have a chance because we will fall back down
        (y < ty0 && yv <= 0)
    if outsideWithNoChance then
        if previouslyHit then Some maxY else None
    else
        let x' = x + xv
        let y' = y + yv
        let xv' =
            if xv > 0 then xv - 1
            elif xv < 0 then xv + 1
            else 0
        let yv' = yv - 1
        let maxY' = max maxY y
        let previouslyHit' =
            if previouslyHit then
                true
            else
                not ( (x < tx0) || 
                      (x > tx1) ||
                      (y < ty0) || 
                      (y > ty1) )
        hitsTarget xv' yv' x' y' maxY' previouslyHit'
    
// yeah this is horrible having to do this by hand
let possibleXVs = seq { 1 .. 50 }
let possibleYVs = seq { -300 .. 900 }

let possibleVs = Seq.allPairs possibleXVs possibleYVs

let successfulVs =
    possibleVs |> Seq.choose (fun (xv,yv) -> hitsTarget xv yv 0 0 0 false)

let partOne =
    successfulVs
        |> Seq.sort
        |> Seq.last

printfn $"%A{partOne}"

let partTwo =
    successfulVs
        |> Seq.length

printfn $"%A{partTwo}"
