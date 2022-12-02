open System.IO

type Shape =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Lose
    | Draw
    | Win

type Round = { OpponentShape: Shape; MyOutcome: Outcome }

type Score = { Opponent: int; Me: int }

let parseFirstColumn =
    function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | x -> failwithf "Unknown value '%s'" x

let parseSecondColumn =
    function
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | x -> failwithf "Unknown value '%s'" x

let parse (line : string) =
    match line.Split(" ") with
    | [| c1; c2 |] ->
        let opponentShape = parseFirstColumn c1
        let myOutcome = parseSecondColumn c2
        { OpponentShape = opponentShape; MyOutcome = myOutcome }
    | _ -> failwithf "Unexpected line '%s'" line

let findShape round =
    match round.OpponentShape, round.MyOutcome with
    | Rock, Lose -> Scissors
    | Rock, Draw -> Rock
    | Rock, Win -> Paper
    | Paper, Lose -> Rock
    | Paper, Draw -> Paper
    | Paper, Win -> Scissors
    | Scissors, Lose -> Paper
    | Scissors, Draw -> Scissors
    | Scissors, Win -> Rock

let shapeScore =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let inverseScore =
    function
    | Win -> Lose
    | Draw -> Draw
    | Lose -> Win

let outcomeScore =
    function
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let playRound round =
    let opponentScore = outcomeScore (inverseScore round.MyOutcome) + shapeScore round.OpponentShape
    let myScore = outcomeScore round.MyOutcome + shapeScore (findShape round)
    { Opponent = opponentScore; Me = myScore }

let solve lines =
    lines
    |> Seq.map (parse >> playRound)
    |> Seq.sumBy (fun outcome -> outcome.Me)

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

solve input
