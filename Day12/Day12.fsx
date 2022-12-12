open System
open System.IO

let split (separator: string) (input: string) = input.Split(separator)

let parse newLine input =
    split newLine input
    |> Seq.indexed
    |> Seq.collect (fun (y, line) ->
        line
        |> Seq.mapi (fun x c ->
            let value =
                match c with
                | 'S' -> -1
                | 'E' -> int 'z' - int 'a' + 1
                | _ -> int c - int 'a'

            ((x, y), value)))
    |> Map.ofSeq

let next (x, y) =
    Set.ofList [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let isReachable c1 c2 = (c2 - c1) <= 1

let reduce f m1 m2 =
    m2
    |> Map.fold (fun acc k2 v2 ->
        let v =
            Map.tryFind k2 m1
            |> Option.map (fun v1 -> f v1 v2)
            |> Option.defaultValue v2
        Map.add k2 v acc) m1

let isStart n = n = -1

let isEnd n = (int 'z' + 1 - int 'a') = n

let iterate map pos visited =
    let current = Map.find pos map
    let nextPositions =
        next pos
        |> Set.filter (fun p -> Map.containsKey p visited |> not)
        |> Set.filter (fun p -> Map.tryFind p map |> Option.map (isReachable current) |> Option.defaultValue false)

    let steps = Map.find pos visited
    let newVisited =
        (visited, nextPositions)
        ||> Set.fold (fun acc p ->
            let newSteps =
                Map.tryFind p acc
                |> Option.map (min (steps + 1))
                |> Option.defaultValue (steps + 1)
            
            Map.add p newSteps acc)

    (nextPositions, newVisited)

let iterateMany map positions visited =
    let results =
        positions
        |> Set.map (fun p -> iterate map p visited)

    ((Set.empty, Map.empty), results)
    ||> Set.fold (fun (ps1, v1) (ps2, v2) -> (Set.union ps1 ps2, (reduce min v1 v2)))

let findPath start map =
    let rec imp goalPosition positions visited =
        match Map.tryFind goalPosition visited with
        | Some steps -> steps
        | None ->
            let (nextPositions, newVisited) = iterateMany map positions visited
            imp goalPosition nextPositions newVisited

    let startPositions =
        map
        |> Map.filter (fun _ elevation -> elevation = start)

    let visited = startPositions |> Map.map (fun _ _ -> 0)
    let goalPosition = Map.findKey (fun _ elevation -> isEnd elevation) map
    imp goalPosition (Set.ofSeq (Map.keys startPositions)) visited

let solve start newLine input =
    let map = input |> parse newLine
    findPath start map

let example = @"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

solve -1 "\n" example

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve -1 Environment.NewLine input
solve 0 Environment.NewLine input