// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines = System.IO.File.ReadLines(fileName) |> Seq.cast<string>

let parseLine (line : string) = 
    let parts = line.Split('|')
    let signalPatterns = parts[0].Trim().Split(' ')
    let outputValues = parts[1].Trim().Split(' ')
    (signalPatterns , outputValues)

let input = (lazy (inputLines |> Seq.map parseLine)).Force()

let isUniqueNumSegments num =
    match num with
    | 2 -> true
    | 3 -> true
    | 4 -> true
    | 7 -> true
    | _ -> false

let partOne = input |> Seq.collect snd |> Seq.filter (fun ovs -> ovs.Length |> isUniqueNumSegments) |> Seq.length

printfn $"%A{partOne}"

let wires = [ 'a' ; 'b' ; 'c' ; 'd' ; 'e' ; 'f' ; 'g' ]
let segmentsToDigits = Map [
    ("abcefg", '0') ; ("cf",      '1') ; ("acdeg", '2') ; ("acdfg",   '3') ; ("bcdf",   '4') ;
    ("abdfg",  '5')  ; ("abdefg", '6') ; ("acf",   '7') ; ("abcdefg", '8') ; ("abcdfg", '9') ]

// from t'internet
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

// work out all the possible mappings (7! is not that big)
let permuatationToMap permutation = permutation |> Seq.zip wires |> Map.ofSeq
let generateMappings = lazy (permutations wires  |> Seq.map permuatationToMap)
let mappings = generateMappings.Force()

let applyMapping (mapping : Map<char,char>) (signalPattern : string) =
    signalPattern.ToCharArray() |> Array.map (fun c -> mapping[c]) |> Array.sort |> System.String

let isLegalMappingFor mapping signalPattern = 
    applyMapping mapping signalPattern |> segmentsToDigits.ContainsKey

let legalMappingsFor signalPatterns =
    mappings |> Seq.filter (
        fun mapping -> signalPatterns |> Seq.forall (fun sp -> isLegalMappingFor mapping sp)
    )

let partTwo = input |> Seq.map (fun entry ->
        let signalPatterns , outputValues = entry
        let legalMapping = legalMappingsFor signalPatterns |> Seq.exactlyOne
        // actually i'm a bit puzzled why i didn't need to invert the mapping
        let intendedOutputValues = outputValues |> Seq.ofArray |> Seq.map (applyMapping legalMapping)
        let outputNumber = intendedOutputValues |> Seq.map (fun segments -> segmentsToDigits[segments]) |> Seq.toArray |> System.String
        outputNumber |> int ) |> Seq.sum

printfn $"""%A{partTwo}"""
