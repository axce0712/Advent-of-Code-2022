open System.IO

type Material =
    | Stone
    | Sand

type Range = { Stone : int; TopSand : int option }

let top r = r.TopSand |> Option.defaultValue r.Stone

let next y ranges =
    let isFilled = ranges |> List.map top |> List.min
    if isFilled = 0 then
        None
    else
        ranges
        |> List.filter (fun r -> top r > y)
        |> List.sortBy (fun r -> r.Stone)
        |> List.tryHead

let contains y ranges =
    ranges
    |> List.exists (fun r -> top r <= y && y <= r.Stone)

let countSand range =
    range.Stone - (range.TopSand |> Option.defaultValue range.Stone)

let findMaterial y { Stone = sy; TopSand = sand } =
    match sand with
    | Some ty when ty <= y && y < sy -> Some Sand
    | _ when sy = y -> Some Stone
    | _ -> None

let split (separator: string) (input: string) =
    input.Split(separator)

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

let floorY map =
    Map.values map
    |> Seq.map (fun rs -> rs |> List.map (fun r -> r.Stone) |> List.max)
    |> Seq.max
    |> (+) 2

let ensureFloor floorY x map =
    let newMap =
        map
        |> Map.change x (function
            | None -> [ { Stone = floorY; TopSand = None } ] |> Some
            | Some rs when rs |> List.exists (fun r -> r.Stone = floorY) |> not -> { Stone = floorY; TopSand = None } :: rs |> Some
            | Some _ as rs -> rs)

    newMap

let changeSand sy ty r =
    if r.Stone = sy then { r with TopSand = Some ty } else r

let rec dropSand floorY (x, y) map =
    match Map.tryFind x map with
    | Some rs ->
        match next y rs with
        | Some range ->
            let ty = top range
            let newMap = map |> ensureFloor floorY (x - 1) |> ensureFloor floorY (x + 1)
            let leftFree =
                Map.tryFind (x - 1) newMap
                |> Option.map ((contains ty) >> not)
                |> Option.defaultValue false

            let rightFree =
                Map.tryFind (x + 1) newMap
                |> Option.map ((contains ty) >> not)
                |> Option.defaultValue false

            match leftFree, rightFree with
            | true, _ -> dropSand floorY (x - 1, ty) newMap
            | _, true -> dropSand floorY (x + 1, ty) newMap
            | _ ->
                newMap
                |> Map.change x (Option.map (List.map (changeSand range.Stone (ty - 1))))
                |> Some
        | None -> None
    | None -> None

let run map =
    let rec imp floorY acc =
        match dropSand floorY (500, 0) acc with
        | Some newAcc -> imp floorY newAcc
        | None -> acc

    let floorY = floorY map
    let flooredMap =
        map
        |> Map.fold (fun acc x _ -> ensureFloor floorY x acc) map

    imp floorY flooredMap

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

let solve lines =
    let map = createMap lines
    let filled = run map
    let sandCount =
        Map.values filled
        |> Seq.sumBy (Seq.sumBy countSand)

    sandCount

let example = @"498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

split "\n" example |> solve

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

#time
solve lines
#time