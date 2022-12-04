open System
open System.IO

type Assignment = { Sections: Set<int> list }

let (|Int|) (input: string) = Int32.Parse input

let (|Range|_|) (input : string) =
    match input.Split("-") with
    | [| Int a; Int b |] -> Some (a, b)
    | _ -> None

let parse (line : string) =
    match line.Split(",") with
    | [| Range (a, b); Range (c, d) |] ->
        let sections = [ Set.ofList [a..b]; Set.ofList [c..d] ]
        { Sections = sections }
    | _ -> failwith $"Cannot parse line '%s{line}'"

let overlap assignment =
    Set.intersectMany assignment.Sections |> Set.isEmpty |> not

let solve lines =
    lines
    |> Seq.map parse
    |> Seq.filter overlap
    |> Seq.length

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
solve input