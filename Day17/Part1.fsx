type Position = int * int

type Jet = Left | Right

type Rock = private { Rocks: Set<Position>; Offset: Position }

type Chamber = private { StoppedRocks: Set<Position>; MaxY: int }

type Repeatable<'a> = private { Source: seq<'a>; Index: int }

module Push =
    let parse = function
        | '<' -> Left
        | '>' -> Right
        | c -> invalidOp $"Unknown char '%c{c}'"

module Repeatable =
    let create source = { Source = source; Index = 0 }

    let next repeatable =
        let item = Seq.item repeatable.Index repeatable.Source
        let newRepeatable =
            { repeatable with
                Index = (repeatable.Index + 1) % Seq.length repeatable.Source }

        item, newRepeatable

module Chamber =
    let empty = { StoppedRocks = Set.empty; MaxY = 0 }

    let tall { MaxY = maxY } = maxY

module Rock =
    let minX { Rocks = rocks; Offset = (ox, _) } =
        rocks |> Seq.map fst |> Seq.min |> (+) ox

    let maxX { Rocks = rocks; Offset = (ox, _) } =
        rocks |> Seq.map fst |> Seq.max |> (+) ox

    let minY { Rocks = rocks; Offset = (_, oy) } =
        rocks |> Seq.map snd |> Seq.min |> (+) oy

    let maxY { Rocks = rocks; Offset = (_, oy) } =
        rocks |> Seq.map snd |> Seq.max |> (+) oy

    let positions { Rocks = rocks; Offset = (ox, oy) } =
        rocks |> Set.map (fun (x, y) -> (x + ox, y + oy))

let push jet ({ Offset = (x, y) } as rock) =
    match jet with
    | Left when Rock.minX rock > 0 -> Some { rock with Offset = (x - 1, y) }
    | Right when Rock.maxX rock < 6 -> Some { rock with Offset = (x + 1, y) }
    | _ -> None

let appear chamber rock =
    let newOffset = (2, chamber.MaxY + 3)
    { rock with Offset = newOffset }

let hit chamber rock =
    let (ox, oy) = rock.Offset
    let actualRockPositions =
        rock.Rocks
        |> Set.map (fun (x, y) -> (x + ox, y + oy))

    let isHit =
        actualRockPositions
        |> Set.exists (fun p -> Set.contains p chamber.StoppedRocks)

    if isHit then None else Some rock

let fallDown rock =
    let (ox, oy) = rock.Offset
    if oy = 0 then
        None
    else
        Some { rock with Offset = (ox, oy - 1) }

type State =
    { Jets: Repeatable<Jet>
      Rocks: Repeatable<Rock>
      Chamber: Chamber }

let update state =
    let rec imp rockStream jetStream rock =
        let (jet, newJetStream) = Repeatable.next jetStream
        let pushedRock =
            push jet rock
            |> Option.bind (hit state.Chamber)
            |> Option.defaultValue rock

        let fallingRock =
            fallDown pushedRock
            |> Option.bind (hit state.Chamber)

        match fallingRock with
        | Some r -> imp rockStream newJetStream r
        | None ->
            let stoppingRockPositions = Rock.positions pushedRock
            let newStoppingRocks = Set.union state.Chamber.StoppedRocks stoppingRockPositions
            let newChamber =
                { StoppedRocks = newStoppingRocks
                  MaxY = max (Rock.maxY pushedRock + 1) (Chamber.tall state.Chamber) }

            { Chamber = newChamber
              Rocks = rockStream
              Jets = newJetStream }

    let (rock, newRocks) = Repeatable.next state.Rocks
    let rockInChamber = appear state.Chamber rock
    imp newRocks state.Jets rockInChamber

let print chamber =
    for y in [ chamber.MaxY + 3 .. -1 .. 0 ] do
        printf "|"
        for x in 0..6 do
            let letter =
                match Seq.tryFind (fun p -> p = (x, y)) chamber.StoppedRocks with
                | Some _ -> "#"
                | None -> "."

            printf "%s" letter
        printfn "|"
    printfn "+-------+"

let solve count rocks input =
    let rockStream = Repeatable.create rocks
    let jetStream = input |> Seq.map Push.parse |> Repeatable.create
    let mutable state =
        { Jets = jetStream
          Rocks = rockStream
          Chamber = Chamber.empty }

    for _ in 1 .. count do
        state <- update state

    Chamber.tall state.Chamber

let rocks = [
    // ####
    { Rocks = Set.ofList [ (0, 0); (1, 0); (2, 0); (3, 0) ]
      Offset = (0, 0) }

    // .#.
    // ###
    // .#.
    { Rocks = Set.ofList [ (0, 1); (1, 0); (1, 1); (1, 2); (2, 1) ]
      Offset = (0, 0) }

    // ..#
    // ..#
    // ###
    { Rocks = Set.ofList [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2) ]
      Offset = (0, 0) }

    // #
    // #
    // #
    // #
    { Rocks = Set.ofList [ (0, 0); (0, 1); (0, 2); (0, 3) ]
      Offset = (0, 0) }

    // ##
    // ##
    { Rocks = Set.ofList [ (0, 0); (0, 1); (1, 0); (1, 1) ]
      Offset = (0, 0) }
]

let example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

solve 2022 rocks example

open System.IO

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve 2022 rocks input