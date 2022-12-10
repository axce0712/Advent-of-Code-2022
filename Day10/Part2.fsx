open System
open System.IO

type Instruction =
    | Noop
    | Addx of int

type State = { Crt : string; SpritePosition : int } 

let initialState = { Crt = "";  SpritePosition = 1 }

let split (separator : string) (input : string) = input.Split(separator)

let parse line =
    match split " " line with
    | [| "noop" |] -> Noop
    | [| "addx"; value |] -> Addx (int value)
    | _ -> invalidArg (nameof line) line

let between lower upper value =
    lower <= value && value <= upper

let cycle state =
    let lower = (state.SpritePosition - 1) % 40
    let upper = (state.SpritePosition + 1) % 40
    let pos = state.Crt.Length % 40
    let pixel =
        if between lower upper pos then "#" else "."

    { state with Crt = state.Crt + pixel }

let addX value state =
    { state with SpritePosition = state.SpritePosition + value }

let run instruction state =
    match instruction with
    | Noop -> cycle state
    | Addx x -> state |> cycle |> cycle |> addX x

let solve lines =
    let flip f x y = f y x
    let finalState =
        lines
        |> List.map parse
        |> List.fold (flip run) initialState

    let crt =
        finalState.Crt
        |> Seq.chunkBySize 40
        |> Seq.map String
        |> String.concat "\n"

    crt

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

solve (Array.toList lines)