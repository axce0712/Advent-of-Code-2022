open System
open System.IO

let parseCompartments (line: string) =
    let items = line |> Seq.toList
    let firstCompartment = items[0..line.Length / 2 - 1]
    let secondCompartment = items[line.Length / 2..]
    (firstCompartment, secondCompartment)

let findSameItem (firstCompartment, secondCompartment) =
    List.find (fun item -> List.contains item secondCompartment) firstCompartment

let getPriority (item: char) =
    if Char.IsLower(item) then
        int item - int 'a' + 1
    else if Char.IsUpper(item) then
        int item - int 'A' + 27
    else
        failwithf "Cannot handle item '%c'" item

let solve lines =
    lines
    |> Seq.sumBy (parseCompartments >> findSameItem >> getPriority)

let input = File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
solve input