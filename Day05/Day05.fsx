open System
open System.IO

type Move =
    { Count: int; StackSource: int; StackTarget: int }

let parseMove (line : string) =
    let words = line.Split(" ")
    let count = int words[1]
    let source = int words[3]
    let target = int words[5]
    { Count = count; StackSource = source; StackTarget = target }

let parseCrate stacks (line : string) =
    stacks
    |> List.choose (fun (i, n) ->
        let candidate = line[i]
        
        if Char.IsLetter(candidate) then
            Some(n, candidate)
        else
            None)

let parseCrates stacks lines =
    let cratePositions =
        lines
        |> Seq.rev
        |> Seq.map (parseCrate stacks)

    let crates =
        (Map.empty, cratePositions)
        ||> Seq.fold (fun acc ps ->
            (acc, ps)
            ||> Seq.fold (fun acc (n, c) ->
                let newCrates =
                    Map.tryFind n acc
                    |> Option.map (fun cs -> c :: cs)
                    |> Option.defaultValue [ c ]
                    
                Map.add n newCrates acc))
            
    crates

let parse newLine (input : string) =
    let parts = input.Split(String.replicate 2 newLine)
    let cratesPartLines = parts[0].Split(newLine)
    let stacks =
        Array.last cratesPartLines
        |> Seq.indexed
        |> Seq.filter (snd >> Char.IsDigit)
        |> Seq.map (fun (i, x) -> (i, int (string x)))
        |> Seq.toList

    let crates = parseCrates stacks (cratesPartLines[..^1])
    let moves =
        parts[1].Split(newLine)
        |> Seq.map (parseMove)
        |> Seq.toList

    (crates, moves)

let move f stacks instruction =
    let stackSource = Map.find instruction.StackSource stacks
    let stackTarget = Map.find instruction.StackTarget stacks
    let newStackTarget =
        (stackSource |> List.take instruction.Count |> f)
        @ stackTarget

    let newStackSource = stackSource |> List.skip instruction.Count

    stacks
    |> Map.add instruction.StackSource newStackSource
    |> Map.add instruction.StackTarget newStackTarget

let solve f (stacks, instructions) =
    let finalStacks = List.fold (move f) stacks instructions

    finalStacks
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map (snd >> List.head)
    |> Seq.toArray
    |> String

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

parse Environment.NewLine input |> solve (List.rev)
parse Environment.NewLine input |> solve id