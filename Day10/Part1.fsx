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

let cycles instructions =
    let rec imp acc cycle x instructions =
        match instructions with
        | [] -> (Map.ofList acc, x)
        | Noop :: is -> imp ((cycle, x) :: acc) (cycle + 1) x is
        | Addx v :: is -> imp ((cycle + 1, x) :: (cycle, x) :: acc) (cycle + 2) (x + v) is

    imp [] 1 1 instructions

let solve lines =
    let signalStrengths, lastX =
        lines
        |> List.map parse
        |> cycles

    [20; 60; 100; 140; 180; 220]
    |> List.map (fun n -> n * Map.find n signalStrengths)
    |> List.sum

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

solve (Array.toList lines)