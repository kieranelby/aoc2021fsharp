// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let reportStrings = System.IO.File.ReadLines(fileName) |> Seq.cast<string>

let numDigits = reportStrings |> Seq.toArray |> fun sa -> sa[0].Length

let allDigitsByPosition ns position = ns |> Seq.map (fun (s : string) -> s.Chars(position))
let matchesDigitAtPosition (s : string) position d = s.Chars(position) = d

let compareDigitAndFreq a b =
    let c = compare (snd a) (snd b)
    // handily in ASCII zero comes before one!
    if c <> 0 then c else compare (fst a) (fst b)

let digitsByFreq (digits : seq<char>) : seq<char> = digits |> Seq.countBy id |> Seq.sortWith compareDigitAndFreq |> Seq.map fst

let mostCommonDigit (digits : seq<char>) : char = digits |> digitsByFreq |> Seq.last

let leastCommonDigit (digits : seq<char>) : char = digits |> digitsByFreq |> Seq.head

let generateRate ns (digitRule : seq<char> -> char) =
    seq { 0 .. numDigits - 1 } |> Seq.map (
        fun (position : int) -> 
            let digits = allDigitsByPosition ns position
            digitRule digits
    ) |> Seq.toList |> System.String.Concat

let epsilon = generateRate reportStrings mostCommonDigit
let gamma = generateRate reportStrings leastCommonDigit

let binToInt s = System.Convert.ToInt32(s, 2);
let printBinaryRatingsAndProduct r1 r2 =
    printfn $"{r1} {r2} => {binToInt r1} * {binToInt r2} => {(binToInt r1) * (binToInt r2)}"

printBinaryRatingsAndProduct epsilon gamma

let rec search (ns : seq<string>) (digitRule : seq<char> -> char) (p : int) =
    let digits = allDigitsByPosition ns p
    let keepDigit = digitRule digits
    let keepFilter s = matchesDigitAtPosition s p keepDigit
    let keepStrings = Seq.where keepFilter ns
    if Seq.length keepStrings = 1 then
        Seq.head keepStrings
    else
        search keepStrings digitRule (p + 1)

let oxyRating = search reportStrings mostCommonDigit 0
let co2Rating = search reportStrings leastCommonDigit 0

printBinaryRatingsAndProduct oxyRating co2Rating
