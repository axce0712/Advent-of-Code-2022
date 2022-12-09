open System

type Direction =
    | Up
    | Down
    | Left
    | Right

type Move = { Direction: Direction; Steps: int }

type Position = int * int

type State = { Head : Position; Tail : Position }

let (|Int|_|) (input : string) =
    match Int32.TryParse input with
    | true, value -> Some value
    | false, _ -> None

let split (separator : string) (input : string) = input.Split(separator)

let parse line =
    match split " " line with
    | [| "U"; Int steps |] -> { Direction = Up; Steps = steps }
    | [| "D"; Int steps |] -> { Direction = Down; Steps = steps }
    | [| "L"; Int steps |] -> { Direction = Left; Steps = steps }
    | [| "R"; Int steps |] -> { Direction = Right; Steps = steps }
    | _ -> invalidArg (nameof line) line

let initialState = { Head = (0, 0); Tail = (0, 0) }

let moveInDirection direction (x, y) =
    match direction with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let applyMove move state =
    let rec imp stateSoFar stepsLeft =
        if stepsLeft = 0 then
            stateSoFar
        else
            let newState =
                { stateSoFar with
                    Head = moveInDirection move.Direction stateSoFar.Head }

            imp newState (stepsLeft - 1)

    imp state move.Steps

applyMove { Direction = Right; Steps = 4 } initialState

let example = @"R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"

split "\n" example |> Array.map parse
