// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
        |> Seq.map (fun l -> l.Trim())
        |> List.ofSeq

let inputAlgo =
    inputLines
    |> List.head 

let inputImageLines =
    inputLines
    |> List.skip 2
    |> Array.ofList

[<AbstractClass>]
type Image() =
    abstract member InterestingRowRange : int * int
    abstract member InterestingColRange : int * int
    abstract member PixelAt : int * int -> bool
    member this.CountLitPixels : int =
        seq {
            for row in fst this.InterestingRowRange .. snd this.InterestingRowRange do
                for col in fst this.InterestingColRange .. snd this.InterestingColRange do
                    yield this.PixelAt(row, col)
        }
        |> Seq.filter id
        |> Seq.length

type InputImage(lines : string[]) =
    inherit Image()
    member this.Lines = lines
    override this.InterestingRowRange = 0 , this.Lines.Length - 1
    override this.InterestingColRange = 0 , this.Lines[0].Length - 1
    override this.PixelAt (row, col) =
        if row < fst this.InterestingRowRange || row > snd this.InterestingRowRange then
            false
        elif col < fst this.InterestingColRange || col > snd this.InterestingColRange then
            false
        else
            let c = this.Lines[row].Chars col
            match c with
            | '.' -> false
            | '#' -> true
            | _ -> failwith "bad input data"

type EnhancedImage(algo : string, baseImage : Image) =
    inherit Image()
    let cache = System.Collections.Generic.Dictionary<(int * int),bool> ()
    member this.Algo = algo
    member this.BaseImage = baseImage
    override this.InterestingRowRange =
        let b0, b1 = this.BaseImage.InterestingRowRange
        b0-1, b1+1
    override this.InterestingColRange =
        let b0, b1 = this.BaseImage.InterestingColRange
        b0-1, b1+1
    override this.PixelAt (row, col) =
        if not (cache.ContainsKey (row,col)) then
            cache.Add((row, col), (this.RawPixelAt(row,col)))
        else
            ()
        cache[(row,col)]

    member this.RawPixelAt (row, col) =
        seq { this.BaseImage.PixelAt(row-1, col-1)
              this.BaseImage.PixelAt(row-1, col)
              this.BaseImage.PixelAt(row-1, col+1)
              this.BaseImage.PixelAt(row,   col-1)
              this.BaseImage.PixelAt(row,   col)
              this.BaseImage.PixelAt(row,   col+1)
              this.BaseImage.PixelAt(row+1, col-1)
              this.BaseImage.PixelAt(row+1, col)
              this.BaseImage.PixelAt(row+1, col+1) }
        |> Seq.map (fun b -> if b then "1" else "0")
        |> String.concat ""
        |> fun s -> System.Convert.ToInt32(s, 2)
        |> fun i -> this.Algo.Chars i
        |> fun c ->
            match c with
            | '.' -> false
            | '#' -> true
            | _ -> failwith "bad input algo"

let rec enhanceN algo n (image : Image) =
    if n = 0 then
        image
    else
        let image' = EnhancedImage(algo, image)
        enhanceN algo (n-1) image'

let partOneImage = enhanceN inputAlgo 2 (InputImage inputImageLines)

printfn $"partOne = {partOneImage.CountLitPixels}"

let partTwoImage = enhanceN inputAlgo 50 (InputImage inputImageLines)

printfn $"partTwo = {partTwoImage.CountLitPixels}"
