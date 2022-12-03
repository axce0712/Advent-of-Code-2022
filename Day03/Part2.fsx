open System
open System.IO

let findBadgeItemType rucksacks =
    rucksacks
    |> Seq.reduce (Set.intersect)
    |> Seq.head

let getPriority (item: char) =
    if Char.IsLower(item) then
        int item - int 'a' + 1
    else if Char.IsUpper(item) then
        int item - int 'A' + 27
    else
        failwithf "Cannot handle item '%c'" item

let solve lines =
    lines
    |> Seq.chunkBySize 3
    |> Seq.sumBy (Seq.map Set.ofSeq >> findBadgeItemType >> getPriority)

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
solve input