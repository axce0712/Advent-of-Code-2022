open System.IO

let split (separator: string) (input: string) =
    input.Split(separator)

let parse line =
    match split " " line with
    | [| "noop" |] -> [| 0 |]
    | [| "addx"; value |] -> [| 0; int value |]
    | _ -> invalidArg (nameof line) line

let pixel pos reg =
    if abs ((pos % 40) - reg) <= 1 then
        "#"
    else
        "."
let solve f lines =
    let cycles =
        lines
        |> Array.collect parse
        |> Array.scan (+) 1

    f cycles

let part1 cycles =
    [ 20..40..240 ]
    |> List.sumBy (fun i -> (Array.item (i - 1) cycles) * i)

let part2 cycles =
    [ 0..239 ]
    |> List.map (fun i -> pixel i (Array.item i cycles))
    |> List.chunkBySize 40
    |> List.map (String.concat "")
    |> String.concat "\n"

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

solve part1 lines
solve part2 lines