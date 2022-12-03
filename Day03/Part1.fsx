open System
open System.IO

let getPriority item =
    if Char.IsLower(item) then
        int item - int 'a' + 1
    else if Char.IsUpper(item) then
        int item - int 'A' + 27
    else
        failwithf "Cannot handle item '%c'" item

let solve lines =
    lines
    |> Seq.map (
        Seq.splitInto 2
        >> Seq.map Set.ofArray
        >> Seq.reduce Set.intersect
        >> Seq.exactlyOne)
    |> Seq.sumBy getPriority

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
solve input