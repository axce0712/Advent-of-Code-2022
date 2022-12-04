open System
open System.IO

type Section = { Start: int; End: int }

type Assignment = { FirstSection: Section; SecontSection: Section }

let isSubset s1 s2 =
    s1.Start <= s2.Start && s1.End >= s2.End

let isOverlapping s1 s2 =
    s1.End >= s2.Start && s1.Start <= s2.End
    
let (|Int|) (input: string) = Int32.Parse input

let parse (line : string) =
    match line.Split(",") |> Array.map (fun section -> section.Split("-")) with
    | [| [| Int a; Int b |]
         [| Int c; Int d |] |] ->
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