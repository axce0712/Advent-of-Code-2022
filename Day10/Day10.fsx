type Instruction =
    | Noop
    | Addx of int

let split (separator : string) (input : string) = input.Split(separator)

let parse line =
    match split " " line with
    | [| "noop" |] -> Noop
    | [| "addx"; value |] -> Addx (int value)
    | _ -> invalidArg (nameof line) line

let example = @"noop
addx 3
addx -5"

split "\n" example |> Seq.map parse