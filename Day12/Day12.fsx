open System
open System.IO

type Path = { Position: int * int; Elevation : char }

let split (separator: string) (input: string) = input.Split(separator)

let parse newLine input =
    split newLine input
    |> Seq.indexed
    |> Seq.collect (fun (y, line) -> line |> Seq.mapi (fun x c -> ((x, y), c)))
    |> Map.ofSeq
    
let isReachable c1 c2 =
    match c1, c2 with
    | 'S', 'a' -> true
    | 'S', _ -> false
    | 'z', 'E' -> true
    | _, 'E' -> false
    | _ -> int c2 - int c1 <= 1 in 

let next (x, y) = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let isStart = (=) 'S'

let isEnd = (=) 'E'

let iterate unvisited path =
    next path.Position
    |> List.choose (fun p ->
        unvisited
        |> Map.tryFind p
        |> Option.filter (isReachable path.Elevation)
        |> Option.map (fun e -> { Position = p; Elevation = e }))

let iterateMany paths unvisited =
    let newPaths =
        paths
        |> List.collect (iterate unvisited)
        |> List.distinct

    let newUnvisited =
        (unvisited, newPaths)
        ||> List.fold (fun acc path -> Map.remove path.Position acc)

    (newPaths, newUnvisited)

let findPath f map =
    let rec imp steps unvisited paths =
        match paths |> List.exists (fun p -> isEnd p.Elevation) with
        | true -> steps
        | false ->
            let (newPaths, newUnvisited) = iterateMany paths unvisited
            imp (steps + 1) newUnvisited newPaths

    let initialPaths =
        map
        |> Map.filter (fun _ elevation -> f elevation)
        |> Map.toList
        |> List.map (fun (position, elevation) -> { Position = position; Elevation = elevation })

    let unvisited =
        initialPaths
        |> List.fold (fun acc path -> Map.remove path.Position acc) map

    imp 0 unvisited initialPaths

let solve f newLine input =
    let map = parse newLine input
    findPath f map

let example =
    @"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

solve isStart "\n" example

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve isStart Environment.NewLine input
solve (function | 'a' -> true | _ -> false) Environment.NewLine input