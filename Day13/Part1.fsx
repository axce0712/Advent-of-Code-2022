open System
open System.IO

type Packet =
    | List of Packet list
    | Integer of int

let split (separator: string) (input: string) = input.Split(separator)

let splitWhile predicate list =
    let rec imp acc list =
        match list with
        | x :: xs when predicate x -> imp (x :: acc) xs
        | _ -> (List.rev acc, list)

    imp [] list

let parsePacket chars =
    let rec imp acc chars =
        match chars with
        | [] -> (acc, [])
        | '[' :: cs ->
            let (inner, left) = imp [] cs
            let newAcc = (List (List.rev inner)) :: acc
            imp newAcc left
        | ']' :: cs -> (acc, cs)
        | ',' :: cs -> imp acc cs
        | c :: cs when Char.IsDigit c ->
            let numbers, left = splitWhile Char.IsDigit cs
            let number = (c :: numbers) |> Seq.map string |> String.concat "" |> int
            let newAcc = Integer number :: acc
            imp newAcc left
        | _ -> invalidArg (nameof chars) (String (Array.ofList chars))

    Seq.toList chars |> imp []  |> fst |> List.head

let parsePart newLine input =
    let [| line1; line2 |] = split newLine input
    (parsePacket line1, parsePacket line2)

let parse newLine input =
    let parts = split (String.replicate 2 newLine) input
    parts |> Array.map (parsePart newLine)

let rec compare p1 p2 =
    match p1, p2 with
    | List [], List [] -> 0
    | List [], List _ -> 1
    | List _, List [] -> -1
    | List (Integer x :: xs), List (Integer y :: ys) ->
        if x < y then 1
        else if x > y then -1
        else compare (List xs) (List ys)
    | List (List x :: xs), List (List y :: ys) ->
        match compare (List x) (List y) with
        | 0 -> compare (List xs) (List ys)
        | v -> v
    | List (Integer x :: xs), List (List y :: ys) ->
        match compare (List [ Integer x ]) (List y) with
        | 0 -> compare (List xs) (List ys)
        | v -> v
    | List (List x :: xs), List (Integer y :: ys) ->
        match compare (List x) (List [ Integer y ]) with
        | 0 -> compare (List xs) (List ys)
        | v -> v
    | _ -> failwithf "Cannot handle %A and %A" p1 p2

let solve newLine input =
    parse newLine input
    |> Array.mapi (fun index (p1, p2) -> (index + 1, compare p1 p2))
    |> Array.choose (fun (pair, result) -> if result = 1 then Some pair else None)
    |> Array.sum

let example = @"[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"

solve "\n" example

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve Environment.NewLine input