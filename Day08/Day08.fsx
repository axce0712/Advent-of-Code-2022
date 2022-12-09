open System
open System.IO

let split (separator : string) (input : string) = input.Split(separator)

let parse newLine input =
    Map.ofList [
        for (y, line) in split newLine input |> Seq.indexed do
            for (x, tree) in line |> Seq.map (string >> int) |> Seq.indexed do
                ((x, y), tree)
    ]

let moveLeft (x, y) = (x - 1, y)

let moveRight (x, y) = (x + 1, y)

let moveUp (x, y) = (x, y - 1)

let moveDown (x, y) = (x, y + 1)

let moves = [ moveLeft; moveUp; moveRight; moveDown ]

let rec isVisible fMove trees pos tree =
    match Map.tryFind pos trees with
    | Some t when t < tree -> isVisible fMove trees (fMove pos) tree
    | Some _ -> false
    | None -> true

let part1 trees =
    trees
    |> Map.filter (fun pos tree ->
        [ moveLeft; moveUp; moveRight; moveDown ]
        |> List.map (fun f -> isVisible f trees (f pos) tree)
        |> List.reduce (||))
    |> Map.count

let visibleTrees fMove trees pos tree =
    let rec imp acc pos =
        match Map.tryFind pos trees with
        | Some t when t < tree -> imp (acc + 1) (fMove pos)
        | Some _ -> acc + 1
        | None -> acc

    imp 0 pos

let part2 trees =
    trees
    |> Map.map (fun pos tree ->
        moves
        |> List.map (fun f -> visibleTrees f trees (f pos) tree)
        |> List.reduce ( * ))
    |> Map.values
    |> Seq.max

let solve f newLine input =
    let trees = parse newLine input
    f trees

let example = @"30373
25512
65332
33549
35390"

solve part1 "\n" example
solve part2 "\n" example

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve part1 Environment.NewLine input
solve part2 Environment.NewLine input