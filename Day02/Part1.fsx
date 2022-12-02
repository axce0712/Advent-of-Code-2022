open System.IO

type Shape =
    | Rock
    | Paper
    | Scissors

type Round = { Opponent: Shape; Me: Shape }

type Outcome = { OpponentScore: int; MyScore: int}

let parseFirstColumn =
    function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | x -> failwithf "Unknown value '%s'" x

let parseSecondColumn =
    function
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

let shapeScore =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let outcomeRound opponent me =
    match opponent, me with
    | Rock, Rock -> 3
    | Rock, Paper -> 6
    | Rock, Scissors -> 0
    | Paper, Rock -> 0
    | Paper, Paper -> 3
    | Paper, Scissors -> 6
    | Scissors, Rock -> 6
    | Scissors, Paper -> 0
    | Scissors, Scissors -> 3

let playRound round =
    let opponentScore = outcomeRound round.Me round.Opponent + shapeScore round.Opponent
    let myScore = outcomeRound round.Opponent round.Me + shapeScore round.Me
    { OpponentScore = opponentScore; MyScore = myScore }

let solve lines =
    lines
    |> Seq.map (parse >> playRound)
    |> Seq.sumBy (fun outcome -> outcome.MyScore)

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

solve input
