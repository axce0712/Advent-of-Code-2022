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

let fullyContain assignment =
    assignment.Sections
    |> List.pairwise
    |> List.map (fun (s1, s2) -> Set.isSubset s1 s2 || Set.isSubset s2 s1)
    |> List.reduce (&&)

let solve lines =
    lines
    |> Seq.map parse
    |> Seq.filter fullyContain
    |> Seq.length

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
solve input