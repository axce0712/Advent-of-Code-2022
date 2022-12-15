open System.IO

type Material =
    | Stone
    | Sand

type X = int

type Y = int

type Range = { Stone : Y; TopSand : Y option }

type State = Map<X, Range list>

let contains y { Stone = sy; TopSand = sand } =
    sand
    |> Option.map (fun ty -> ty <= y && y <= y)
    |> Option.defaultValue (sy = y)

let findMaterial y { Stone = sy; TopSand = sand } =
    match sand with
    | Some ty when ty <= y && y < sy -> Some Sand
    | _ when sy = y -> Some Stone
    | _ -> None

let top range =
    range.TopSand |> Option.defaultValue range.Stone

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
    |> Seq.pairwise
    |> Seq.collect (fun (p1, p2) -> makeLine p1 p2)

let createMap lines =
    lines
    |> Seq.collect (parse >> createStones)
    |> Seq.groupBy fst
    |> Seq.map (fun (x, ps) ->
        let ranges =
            ps
            |> Seq.map (fun (_, y) -> { Stone = y; TopSand = None })
            |> Seq.distinct
            |> Seq.toList

        (x, ranges))
    |> Map.ofSeq

let tryFindBlocker (x, y) stones =
    stones
    |> Array.filter (fun { Position = (sx, sy) } -> x = sx && sy > y)
    |> Array.sortBy (fun { Position = (_, y) } -> y)
    |> Array.tryHead

let rec dropSand (x, y) (stones : Map<int, Range list>) =
    match Map.tryFind x stones with
    | Some rs ->
        match rs |> List.tryFind (contains y) |> Option.map top with
        | Some ty ->
            let left =
                Map.tryFind (x - 1) stones
                |> Option.bind (fun rs -> rs |> List.tryFind (fun r -> contains ty r))

            let right =
                Map.tryFind (x + 1) stones
                |> Option.bind (fun rs -> rs |> List.tryFind (fun r -> contains ty r))

            match left, right with
            | None, _ -> dropSand (x - 1, ty) stones
            | _, None -> dropSand (x + 1, ty) stones
            | Some _, Some _ -> Some (x, ty - 1)
        | None -> None
    | None -> None

let print stones =
    let xs = Map.keys stones |> Seq.toArray
    let ys =
        Map.values stones
        |> Seq.collect (List.map (fun { Stone = y } -> y))

    let minX = Array.min xs
    let maxX = Array.max xs
    let minY = 0
    let maxY = Seq.max ys
    for y in Seq.unfold (fun y -> if y <= maxY then Some (y, y + 1) else None) minY do
        for x in Seq.unfold (fun x -> if x <= maxX then Some (x, x + 1) else None) minX do
            let symbol =
                stones
                |> Map.tryFind x
                |> Option.bind (fun rs -> rs |> List.tryPick (fun r -> findMaterial y r))
                |> Option.map (function | Sand -> "o" | Stone -> "#")
                |> Option.defaultValue "."

            printf "%s" symbol

        printfn ""

let example = @"498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

split "\n" example
|> createMap
|> print

|> step
|> step
|> step
|> step
|> step
|> print

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

#time
solve lines
#time