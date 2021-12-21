// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
        |> Seq.map (fun l -> l.Trim())

type SnailfishElement =
    | Regular of int
    | Snailfish of SnailfishNumber
and SnailfishNumber = SnailfishElement * SnailfishElement

type SnailfishToken =
    | Digits of int
    | Comma
    | Opening
    | Closing

let lexSnailfishNumber (s : string) =
    s.ToCharArray()
        |> Seq.ofArray
        |> Seq.map (fun c ->
            match c with
            | '[' -> Opening
            | ']' -> Closing
            | ',' -> Comma
            | d when d >= '0' && d <= '9' -> d |> string |> int |> Digits
            | _ -> failwith "expected [, ], or 0-9" )
        |> Seq.filter (fun t -> t <> Comma) // they don't really add much
        |> Seq.toList

let rec parseSnailfishTokens (ts : list<SnailfishToken>) =
    if List.head ts <> Opening then failwith "expected [" else ()
    let ts = ts.GetSlice(Some 1, None)
    let left, ts = parseSnailfishElement ts
    let right, ts = parseSnailfishElement ts
    if List.head ts <> Closing then failwith "expected ]" else ()
    let ts = ts.GetSlice(Some 1, None)
    (SnailfishNumber(left, right), ts)
and parseSnailfishElement (ts : list<SnailfishToken>) : (SnailfishElement * list<SnailfishToken>) =
    match List.head ts with
    | Opening -> 
        let n, r = parseSnailfishTokens ts
        (Snailfish n, r)
    | Digits d -> (Regular d, ts.GetSlice(Some 1, None))
    | _ -> failwith "expected [ or 0..9"

let rec formatToken t =
    match t with
    | Opening -> "["
    | Closing -> "]"
    | Digits d -> d |> string
    | Comma -> ","

let rec numToTokens (num : SnailfishNumber) =
    let l, r = num
    seq {
        Opening
        yield! (elemToTokens l)
        yield! (elemToTokens r)
        Closing
    } |> Seq.toList
and elemToTokens (elem : SnailfishElement) =
    match elem with
    | Regular r -> seq { Digits r }
    | Snailfish s -> numToTokens s

let formatNum n =
    n |> numToTokens |> Seq.map formatToken |> Array.ofSeq |> (fun s -> System.String.Join(" ", s))

let rec split num =
    let le, re = num
    let makePair n =
        Snailfish (SnailfishNumber (Regular (n/2), Regular ((n+1)/2)))
    let le, ls =
        match le with
        | Regular n when n >= 10 -> ((makePair n), true)
        | Regular _ -> (le, false)
        | Snailfish sn ->
            let foo, splitted = split sn
            (Snailfish foo, splitted)
    if ls then
        (SnailfishNumber (le, re), true)
    else
        let re, rs =
            match re with
            | Regular n when n >= 10 -> ((makePair n), true)
            | Regular _ -> (re, false)
            | Snailfish sn ->
                let foo, splitted = split sn
                (Snailfish foo, splitted)
        if rs then
            (SnailfishNumber (le, re), true)
        else
            (num, false)


let rec addToFirstRegular inc remaining =
    match remaining with
    | [] -> []
    | head :: remaining ->
        match head with
        | Digits n -> (Digits (n + inc)) :: remaining
        | _ -> head :: (addToFirstRegular inc remaining)

let rec splitTokens ts =
    let makePair n =
        [
            Opening
            Digits (n/2)
            Digits ((n+1)/2)
            Closing
        ]
    match ts with
    | [] -> ([], false)
    | head :: tail ->
        match head with 
        | Digits d when d >= 10 ->
            (List.append (makePair d) tail, true)
        | _ ->
            let tail', splitted = splitTokens tail
            (head :: tail', splitted)

let split2 num =
    let tokens', splitted =
        num
            |> numToTokens
            |> splitTokens
    if splitted then (tokens' |> parseSnailfishTokens |> fst, true) else (num, false)

let rec explodeTokens depth beforeRev remaining =
    match remaining with
    | [] -> (List.rev beforeRev, false)
    | head :: remaining ->
        if depth = 5 then
            match head with 
            | Digits leftNum ->
                match remaining with
                | (Digits rightNum) :: remaining ->
                    let explodedLeft =
                        List.skip 1 beforeRev // skip the [
                        |> addToFirstRegular leftNum
                        |> List.rev
                    let explodedRight =
                        List.skip 1 remaining // skip the ]
                        |> addToFirstRegular rightNum
                    let explodedTokens =
                        List.append
                            (List.append explodedLeft [ Digits 0 ])
                            explodedRight
                    (explodedTokens, true)
                | _ -> failwith "expected digits to come in pairs"
            | _ -> failwith "expected exploding pair to be regular"
        else
            let depth' =
                match head with
                | Opening -> depth + 1
                | Closing -> depth - 1
                | _ -> depth
            explodeTokens depth' (head :: beforeRev) remaining

let explode num =
    let tokens', exploded =
        num
            |> numToTokens
            |> explodeTokens 0 List.empty
    if exploded then (tokens' |> parseSnailfishTokens |> fst, true) else (num, false)

let rec reduceNumber num =
    let num', exploded = explode num
    //if exploded then printfn $"! %A{num |> formatNum} exploded to\n> %A{num' |> formatNum}" else ()
    if exploded then
        reduceNumber num'
    else
        let num'', splitted = split2 num'
        //if splitted then printfn $"s %A{num' |> formatNum} splitted to\n> %A{num'' |> formatNum}" else ()
        if splitted then
            reduceNumber num''
        else
            num

let add a b =
    SnailfishNumber (Snailfish a, Snailfish b)
    |> reduceNumber

let rec magOfNum num =
    3 * mag (fst num) + 2 * mag (snd num)
and mag elem =
    match elem with
    | Regular rn -> rn
    | Snailfish sn -> magOfNum sn

let sum = inputLines |> Seq.map lexSnailfishNumber |> Seq.map parseSnailfishTokens |> Seq.map fst |> Seq.reduce add

printfn $"partOne=%A{sum |> magOfNum}"

let snailfishNumbers =  inputLines |> Seq.map lexSnailfishNumber |> Seq.map parseSnailfishTokens |> Seq.map fst

let partTwo =
    Seq.allPairs snailfishNumbers snailfishNumbers
        |> Seq.filter (fun (a, b) -> (formatNum a) <> (formatNum b))
        |> Seq.map (fun (a, b) -> add a b)
        |> Seq.map magOfNum
        |> Seq.sort
        |> Seq.last

printfn $"partTwo=%A{partTwo}"
