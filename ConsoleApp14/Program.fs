// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
        |> Seq.map (fun l -> l.Trim())

let parsePairInsertionRule (line : string) =
    let parts = line.Split(" -> ")
    (parts[0], parts[1])

let polymerTemplate = inputLines |> Seq.head

let pairInsertionRules =
    inputLines
        |> Seq.skipWhile (fun l -> l.Contains " -> " |> not)
        |> Seq.map parsePairInsertionRule
        |> Map.ofSeq

// part one

let applyRulesOnce rules (template : string) =
    let len = template.Length
    let firsts = template.Substring(0, len-1).ToCharArray()
    let seconds = template.Substring(1, len-1).ToCharArray()
    let folder acc first second =
        let pair = $"{first}{second}"
        let insertion =
            match Map.tryFind pair rules with
            | Some x -> x
            | None -> ""
        $"{acc}{first}{insertion}"
    let nearlyRight = Seq.fold2 folder "" firsts seconds
    $"{nearlyRight}{Seq.last seconds}"

let rec applyRulesN n rules template =
    //printfn $"""n=%A{n} template=%A{template}"""
    if n = 0 then
        template
    else
        let template' = applyRulesOnce rules template
        applyRulesN (n-1) rules template'

let leastCommonQuantity polymer =
    polymer
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.min

let mostCommonQuantity polymer =
    polymer
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.max
    
let partOne =
    let polymer = applyRulesN 10 pairInsertionRules polymerTemplate
    (mostCommonQuantity polymer) - (leastCommonQuantity polymer)

printfn $"""partOne=%A{partOne}"""

// part two

// 'orrbile hack; add a bogus char at the start so at the end when we map back from
// counts of pairs to counts of elements we can avoid double-counting overlapping
// pairs by simply only considering each pair as the second element (if we didn't
// add the bogus char at the start then we would undercount the first char!)
let polymerPairCounts =
    $"*{polymerTemplate}"
        |> Seq.pairwise
        |> Seq.map (fun (a,b) -> $"{a}{b}")
        |> Seq.countBy id
        |> Seq.map (fun (pair, count) -> (pair, count |> bigint))

let applyCountRulesOnce rules (templatePairCounts : seq<string * bigint>) =
    templatePairCounts
        |> Seq.collect (
            fun (pair, count) ->
                match Map.tryFind pair rules with
                | Some x -> seq { ($"{pair.Chars(0)}{x}", count) ; ($"{x}{pair.Chars(1)}", count) }
                | None -> seq { (pair,count) })
        |> Seq.groupBy fst
        |> Seq.map (fun (a, entries) -> (a, (entries |> Seq.map snd |> Seq.sum)) )

let rec applyCountRulesN n rules templatePairCounts =
    printfn $"""n=%A{n} template=%A{templatePairCounts} sum={templatePairCounts |> Seq.map snd |> Seq.sum}"""
    if n = 0 then
        templatePairCounts
    else
        let templatePairCounts' = applyCountRulesOnce rules templatePairCounts
        applyCountRulesN (n-1) rules templatePairCounts'

let pairCountsToElementCounts (templatePairCounts : seq<string * bigint>) =
    templatePairCounts
        |> Seq.map (fun (pair, count) -> (pair.Chars(1), count))
        |> Seq.groupBy fst
        |> Seq.map (fun (a, entries) -> (a, (entries |> Seq.map snd |> Seq.sum)) )

let partTwo =
    let pairCounts = applyCountRulesN 40 pairInsertionRules polymerPairCounts
    printfn $"""%A{pairCounts |> pairCountsToElementCounts}"""
    let mostCommon = pairCounts |> pairCountsToElementCounts |> Seq.map snd |> Seq.max
    let leastCommon = pairCounts |> pairCountsToElementCounts |> Seq.map snd |> Seq.min
    mostCommon - leastCommon

printfn $"""partTwo=%A{partTwo}"""
