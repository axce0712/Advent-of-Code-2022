open System.IO

let findStartOfPacket disctinctCount buffer =
    buffer
    |> Seq.windowed disctinctCount
    |> Seq.findIndex (fun xs -> Seq.distinct xs |> Seq.length = disctinctCount)
    |> (+) disctinctCount

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

findStartOfPacket 4 input
findStartOfPacket 14 input