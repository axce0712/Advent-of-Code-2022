open System
open System.IO

type Direction =
    | Up
    | Down
    | Left
    | Right

type Move = { Direction: Direction; Steps: int }

type Position = { X: int; Y: int }

type State =
    { Head : Position
      Tails : Position list
      TailPositions: Set<Position> }

module Position =
    let zero = { X = 0; Y = 0 }
    
    let distance { X = x1; Y = y1 } { X = x2; Y = y2 } =
        { X = (x2 - x1); Y = (y2 - y1) }

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

let initialState tailCount =
    { Head = Position.zero
      Tails = List.replicate tailCount Position.zero
      TailPositions = Set.singleton Position.zero }

let moveInDirection direction pos =
    match direction with
    | Up -> { pos with Y = pos.Y + 1 }
    | Down -> { pos with Y = pos.Y - 1 }
    | Left -> { pos with X = pos.X - 1 }
    | Right -> { pos with X = pos.X + 1 }

let readjust p1 p2 =
    match Position.distance p1 p2 with
    | { X = 2; Y = 2 } -> { X = p2.X - 1; Y = p2.Y - 1 }
    | { X = 2; Y = -2 } -> { X = p2.X - 1; Y = p2.Y + 1 }
    | { X = -2; Y = 2 } -> { X = p2.X + 1; Y = p2.Y - 1 }
    | { X = -2; Y = -2 } -> { X = p2.X + 1; Y = p2.Y + 1 }
    | { X = 2 } -> { p2 with X = p2.X - 1 }
    | { X = -2 } -> { p2 with X = p2.X + 1 }
    | { Y = 2 } -> { p2 with Y = p2.Y - 1 }
    | { Y = -2 } -> { p2 with Y = p2.Y + 1 }
    | _ -> p1

let applyMove move state =
    let rec imp stateSoFar stepsLeft =
        if stepsLeft = 0 then
            stateSoFar
        else
            let newHead = moveInDirection move.Direction stateSoFar.Head
            let newTails, lastPosition =
                (newHead, stateSoFar.Tails)
                ||> List.mapFold (fun acc pos ->
                    let newPos = readjust pos acc
                    newPos, newPos)

            let newTailPositions = Set.add lastPosition stateSoFar.TailPositions
            let newState =
                { Head = newHead
                  Tails = newTails
                  TailPositions = newTailPositions }

            imp newState (stepsLeft - 1)

    imp state move.Steps

let flip f x y = f y x

let solve tailCount lines =
    let moves = Seq.map parse lines
    let finalState = Seq.fold (flip applyMove) (initialState tailCount) moves
    Set.count finalState.TailPositions

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadLines

solve 1 lines
solve 9 lines