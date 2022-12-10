open System.IO

type Instruction =
    | Noop
    | Addx of int

let split (separator : string) (input : string) = input.Split(separator)

let parse line =
    match split " " line with
    | [| "noop" |] -> Noop
    | [| "addx"; value |] -> Addx (int value)
    | _ -> invalidArg (nameof line) line

let solve lines =
    let cycles =
        lines
        |> Array.map parse
        |> Array.collect (function | Noop -> [| 0 |] | Addx v -> [| 0; v |])
        |> Array.scan (+) 1

    [20 .. 40 .. 220]
    |> List.sumBy (fun i -> i * Array.item (i - 1) cycles)

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

solve lines