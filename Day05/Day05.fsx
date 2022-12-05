open System
open System.IO

type Move = { Count: int; SourceStack: int; TargetStack: int }

let parseStacks lines =
    let extracted =
        lines
        |> Seq.map (fun line ->
            Seq.indexed line
            |> Seq.filter (fun (i, _) -> ((i - 1) % 4) = 0)
            |> Seq.map snd
            |> Seq.toList)

    let stacks =
        List.transpose extracted
        |> List.map (fun xs ->
            let (y :: ys) =
                List.rev xs
                |> List.filter (fun x -> x <> ' ')

            (int (string y), List.rev ys))
    
    Map.ofList stacks

let parseMove (line : string) =
    let words = line.Split(" ")
    let count = int words[1]
    let source = int words[3]
    let target = int words[5]
    { Count = count; SourceStack = source; TargetStack = target }

let parse newLine (input : string) =
    let parts = input.Split(String.replicate 2 newLine)
    let stacks = parseStacks (parts[0].Split(newLine))
    let moves = parts[1].Split(newLine) |> Seq.map parseMove |> Seq.toList
    (stacks, moves)

let move f stacks instruction =
    let sourceStack = Map.find instruction.SourceStack stacks
    let targetStack = Map.find instruction.TargetStack stacks
    let taken, left = List.splitAt instruction.Count sourceStack

    stacks
    |> Map.add instruction.SourceStack left
    |> Map.add instruction.TargetStack ((f taken) @ targetStack)

let solve f (stacks, instructions) =
    let finalStacks = List.fold (move f) stacks instructions
    let topCrates =
        Map.toSeq finalStacks
        |> Seq.sortBy fst
        |> Seq.map (snd >> List.head)
        |> Seq.toArray
    
    String topCrates

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

parse Environment.NewLine input |> solve (List.rev)
parse Environment.NewLine input |> solve id