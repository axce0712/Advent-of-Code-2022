open System
open System.IO

type Section = { Start: int; End: int }

type Assignment = { FirstSection: Section; SecontSection: Section }

let isSubset s1 s2 =
    let mutable overlapping = true
    for i in s1.Start..s1.End do
        let mutable exists = false
        for j in s2.Start..s2.End do
            exists <- exists || (i = j)
        overlapping <- overlapping && exists
    overlapping

let isOverlapping s1 s2 =
    let mutable overlapping = false
    for i in s1.Start..s1.End do
        for j in s2.Start..s2.End do
            overlapping <- overlapping || (i = j)
    overlapping

let (|Int|) (input: string) = Int32.Parse input

let (|Range|_|) (input : string) =
    match input.Split("-") with
    | [| Int a; Int b |] -> Some (a, b)
    | _ -> None

let parse (line : string) =
    match line.Split(",") with
    | [| Range (a, b); Range (c, d) |] ->
        { FirstSection = { Start = a; End = b }
          SecontSection = { Start = c; End = d } }
    | _ ->
        failwith $"Cannot parse line '%s{line}'"

let solve f lines =
    lines
    |> Seq.map parse
    |> Seq.filter (fun a -> f a.FirstSection a.SecontSection)
    |> Seq.length

#time
let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
printfn "%i" (solve (fun s1 s2 -> isSubset s1 s2 || isSubset s2 s1) input)
printfn "%i" (solve isOverlapping input)
#time