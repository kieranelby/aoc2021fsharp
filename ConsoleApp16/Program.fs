// Advent of Code 2021 - F# noob

//let fileName = "example.txt"
let fileName = "input.txt"
let inputLines =
    System.IO.File.ReadLines(fileName)
        |> Seq.cast<string>
        |> Seq.map (fun l -> l.Trim())

let inputHexChars =
    inputLines
        |> Seq.head
        |> fun l -> l.ToCharArray()
        |> List.ofArray

let hexCharToBinaryChars hexChar =
    hexChar
        |> string
        |> fun hs -> System.Convert.ToInt64(hs, 16)
        |> fun n -> System.Convert.ToString(n, 2)
        |> fun bs -> bs.PadLeft(4, '0')
        |> fun bs -> bs.ToCharArray()
        |> List.ofArray

let inputBinaryChars =
    inputHexChars
        |> List.collect hexCharToBinaryChars

let parseBinaryNumber length bits  =
    let head = List.take length bits
    let bits = List.skip length bits
    let hs = head |> Array.ofList |> System.String
    let num = System.Convert.ToInt64(hs, 2) |> bigint
    (num, bits)
    
let parseHeader bits =
    let version, bits = parseBinaryNumber 3 bits
    let typeId, bits = parseBinaryNumber 3 bits
    ((version, typeId), bits)

let rec parseLiteralValue bits acc =
    let prefix, bits = parseBinaryNumber 1 bits
    let num, bits = parseBinaryNumber 4 bits
    let res = acc * (bigint 16) + num
    if prefix = (bigint 0) then
        (res, bits)
    else
        parseLiteralValue bits res

type PacketPayload = LiteralValue of bigint | Operator of list<Packet>
and Packet = {Version: bigint; TypeId: bigint; Payload: PacketPayload}

let rec parsePackets packetParser bits maxPackets maxBits acc =
    if maxPackets <= 0 || maxBits <= 0 then
        (acc, bits)
    else
        let packet, bits' = packetParser bits
        let bitsRead = (List.length bits) - (List.length bits')
        parsePackets packetParser bits' (maxPackets-1) (maxBits-bitsRead) (List.append acc [ packet ])

let parseLengthAndSubPackets packetParser bits =
    let lengthTypeId, bits = parseBinaryNumber 1 bits
    if lengthTypeId = (bigint 0) then
        let subPacketsLength, bits = parseBinaryNumber 15 bits
        parsePackets packetParser bits System.Int32.MaxValue (int subPacketsLength) List.empty
    else
        let numSubPackets, bits = parseBinaryNumber 11 bits
        parsePackets packetParser bits (int numSubPackets) System.Int32.MaxValue  List.empty
    
let rec parsePacket bits =
    let (version, typeId), bits = parseHeader bits
    if typeId = 4 then
        let literalValue, bits = parseLiteralValue bits 0
        let packet = { Version=version; TypeId=typeId; Payload=LiteralValue literalValue }
        (packet, bits)
    else
        let subPackets, bits = parseLengthAndSubPackets parsePacket bits
        let packet = { Version=version; TypeId=typeId; Payload=Operator subPackets }
        (packet, bits)

let outerPacket =
    inputBinaryChars
        |> parsePacket
        |> fst

let rec sumVersions packet =
    match packet.Payload with 
    | Operator subPackets -> packet.Version + (subPackets |> List.map sumVersions |> List.sum)
    | LiteralValue _ -> packet.Version

let rec evaluate packet =
    match packet.Payload with 
    | LiteralValue v -> v
    | Operator subPackets ->
        match int packet.TypeId with
        | 0 -> subPackets |> List.map evaluate |> List.sum
        | 1 -> subPackets |> List.map evaluate |> List.fold (fun a b -> a * b) (bigint 1)
        | 2 -> subPackets |> List.map evaluate |> List.min
        | 3 -> subPackets |> List.map evaluate |> List.max
        | 5 -> subPackets |> List.map evaluate |> fun vs -> if vs[0] > vs[1] then (bigint 1) else (bigint 0)
        | 6 -> subPackets |> List.map evaluate |> fun vs -> if vs[0] < vs[1] then (bigint 1) else (bigint 0)
        | 7 -> subPackets |> List.map evaluate |> fun vs -> if vs[0] = vs[1] then (bigint 1) else (bigint 0)
        | _ -> failwith "unknown operator"

printfn $"%A{sumVersions outerPacket}"
printfn $"%A{evaluate outerPacket}"

