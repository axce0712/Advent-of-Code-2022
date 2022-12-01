open System
open System.IO

let input = File.ReadAllText (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))

let parts = input.Split(String.replicate 2 Environment.NewLine)

let caloriesPerElve =
    parts
    |> Seq.map (fun part -> part.Split(Environment.NewLine) |> Seq.sumBy int)
    |> Seq.toList

let topThree =
    caloriesPerElve
    |> List.sortDescending
    |> List.take 3

let mostCalories = List.sum topThree
    