open System.IO

let findStartOfPacket disctinctCount buffer =
    buffer
    |> Seq.windowed disctinctCount
    |> Seq.indexed
    |> Seq.tryFind (fun (_, xs) -> Seq.distinct xs |> Seq.length = disctinctCount)
    |> Option.map (fst >> ((+) disctinctCount))

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

findStartOfPacket 4 input
findStartOfPacket 14 input