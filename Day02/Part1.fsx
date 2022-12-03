open System.IO

type Shape =
    | Rock
    | Paper
    | Scissors

type Outcome =
    | Lose
    | Draw
    | Win

type Round = { Opponent: Shape; Me: Shape }

type Score = { OpponentScore: int; MyScore: int }

let parseFirstColumn = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | x -> failwithf "Unknown value '%s'" x

let parseSecondColumn = function
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | x -> failwithf "Unknown value '%s'" x

let parse (line : string) =
    match line.Split(" ") with
    | [| c1; c2 |] ->
        let opponentShape = parseFirstColumn c1
        let myShape = parseSecondColumn c2
        { Opponent = opponentShape; Me = myShape }
    | _ -> failwithf "Unexpected line '%s'" line

let shapeScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let outcomeRound opponent me =
    match opponent, me with
    | Rock, Rock -> Draw
    | Rock, Paper -> Win
    | Rock, Scissors -> Lose
    | Paper, Rock -> Lose
    | Paper, Paper -> Draw
    | Paper, Scissors -> Win
    | Scissors, Rock -> Win
    | Scissors, Paper -> Lose
    | Scissors, Scissors -> Draw

let outcomeScore = function
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let playRound round =
    let opponentScore =
        outcomeScore (outcomeRound round.Me round.Opponent)
        + shapeScore round.Opponent

    let myScore =
        outcomeScore (outcomeRound round.Opponent round.Me)
        + shapeScore round.Me
        
    { OpponentScore = opponentScore; MyScore = myScore }

let solve lines =
    lines
    |> Seq.map (parse >> playRound)
    |> Seq.sumBy (fun outcome -> outcome.MyScore)

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
solve input