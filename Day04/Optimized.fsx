open System.IO

type Section = { Start: int; End: int }

type Assignment = { First: Section; Second: Section }

let isSubset s1 s2 =
    s1.Start <= s2.Start && s1.End >= s2.End

let containsFully s1 s2 =
    isSubset s1 s2 || isSubset s2 s1

let isOverlapping s1 s2 =
    s1.End >= s2.Start && s1.Start <= s2.End
    
let parse (line : string) = // "3-78,73-77"
    let [| [| s1; e1 |]; [| s2; e2 |] |] =
        line.Split(",") // ["3-78";"73-77"]
        |> Array.map (fun x -> x.Split("-")) // [["3";"78"];["73";"77"]]
        
    { First = { Start = int s1; End = int e1 }
      Second = { Start = int s2; End = int e2 } }

let solve f lines =
    lines
    |> Seq.map parse
    |> Seq.filter (fun { First = s1; Second = s2 } -> f s1 s2)
    |> Seq.length

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
printfn "%i" (solve containsFully input)
printfn "%i" (solve isOverlapping input)
