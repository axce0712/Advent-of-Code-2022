open System.IO

type Material =
    | Stone
    | Sand

let split (separator: string) (input: string) = input.Split(separator)

let parseCoordiante input =
    let [| x; y |] = split "," input
    (int x, int y)

let parse line =
    split " -> " line
    |> Array.map parseCoordiante

let makeLine ((x1, y1) as p1) ((x2, y2) as p2) =
    if x1 = x2 then
        [| for y in (min y1 y2) .. (max y1 y2) -> (x1, y) |]
    else if y1 = y2 then
        [| for x in (min x1 x2) .. (max x1 x2) -> (x, y1) |]
    else
        invalidOp $"Cannot handle positions %A{p1} and %A{p2}"

let createStones coordinates =
    coordinates
    |> Array.pairwise
    |> Array.collect (fun (p1, p2) -> makeLine p1 p2)
    |> Array.distinct

let createMap lines =
    lines
    |> Array.collect (parse >> createStones)
    |> Array.fold (fun acc pos -> Map.add pos Stone acc) Map.empty

let rec dropSand (x, y) map =
    if not (Seq.exists (fun (_, y2) -> y2 > y) (Map.keys map)) then
        None
    else
        match Map.tryFind (x, y + 1) map with
        | None -> dropSand (x, y + 1) map
        | Some _ ->
            match Map.tryFind (x - 1, y + 1) map, Map.tryFind (x + 1, y + 1) map with
            | None, _ -> dropSand (x - 1, y + 1) map
            | _, None -> dropSand (x + 1, y + 1) map
            | Some _, Some _ -> Some (x, y)
    
let produceSand map =
    let rec imp (x, y) map =
        match dropSand (x, y) map with
        | Some p ->
            let newMap = Map.add p Sand map
            imp (x, y) newMap
        | None -> map

    imp (500, 0) map

let print map =
    let positions = Seq.toList (Map.keys map)
    let xs = List.map fst positions
    let ys = List.map snd positions
    let minX = List.min xs
    let maxX = List.max xs
    let maxY = List.max ys
    for y in Seq.unfold (fun y -> if y <= maxY then Some (y, y + 1) else None) 0 do
        for x in Seq.unfold (fun x -> if x <= maxX then Some (x, x + 1) else None) minX do
            match Map.tryFind (x, y) map with
            | Some Stone -> printf "#"
            | Some Sand -> printf "o"
            | None -> printf "."

        printfn ""

let solve lines =
    let sandedMap = createMap lines |> produceSand
    (0, sandedMap)
    ||> Map.fold (fun  acc _ material ->
        match material with
        | Sand -> acc + 1
        | Stone -> acc)

let example = @"498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

split "\n" example |> solve

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

#time
solve lines
#time