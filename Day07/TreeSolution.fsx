open System
open System.IO

type Tree<'a, 'b> =
    | Node of 'a * Tree<'a, 'b> list
    | Leaf of 'b

module Tree =
    let leaf = Leaf
    
    let node x xs = Node (x, xs)

    let rec cata fd ff = function
        | Leaf x -> ff x
        | Node (x, xs) -> xs |> List.map (cata fd ff) |> fd x

    let choose f = cata (fun x -> List.choose id >> node x >> Some) (f >> Option.map Leaf)

    let bimap f g = cata (f >> node) (g >> leaf)

    let map f = bimap id f

    let bifold f g z t =
        let flip f x y = f y x
        cata (fun x xs -> flip f x >> List.fold (>>) id xs) (flip g) t z

type FileItem = { Name: string; Size: int }

type DirectoryItem = { Name: string }

type Output =
    | Directory of Name : string
    | File of Name : string * Size : int

let (|Int|_|) (input : string) =
    match Int32.TryParse input with
    | true, value -> Some value
    | false, _ -> None

let split (separator : string) (input : string) =
    input.Split(separator) |> List.ofSeq

let words = split " "

let parse lines =
    let parseFilesAndDirs lines =
        let rec imp acc lines =
            match lines with
            | [ "dir"; name ] :: next -> imp ((Directory name) :: acc) next
            | [ Int size; name ] :: next -> imp ((File (name, size)) :: acc) next
            | next -> (List.rev acc, next)

        imp [] lines
        
    let rec imp dirStack dirs lines =
        match lines with
        | [] -> dirs
        | [ "$"; "cd"; "/" ] :: next -> imp [] dirs next
        | [ "$"; "cd"; ".." ] :: next -> imp (List.tail dirStack) dirs next
        | [ "$"; "cd"; d ] :: next -> imp (d :: dirStack) dirs next
        | [ "$"; "ls" ] :: next ->
            let (filesAndDirs, left) = parseFilesAndDirs next
            let newDirs = Map.add dirStack filesAndDirs dirs
            imp dirStack newDirs left
        | xs -> invalidOp (String.concat " " (List.head xs))

    imp [] Map.empty lines

let rec makeTree dirs =
    let rec imp dirStack dirs = function
        | File (name, size) -> Tree.leaf { Name = name; Size = size }
        | Directory name ->
            let dirAndFiles = Map.find (name :: dirStack) dirs
            let branches = dirAndFiles |> List.map (imp (name :: dirStack) dirs)
            Tree.node { Name = name } branches

    let branches =
        Map.find [] dirs
        |> List.map (imp [] dirs)

    Tree.node { Name = "/" } branches

let rec totalSize = function
    | Node (_, xs) -> List.sumBy totalSize xs
    | Leaf x -> x

let solve fSolve newLine input =
    let dirs = input |> split newLine |> List.map words |> parse
    let totalSizes =
        makeTree dirs
        |> Tree.cata
            (fun _ sizes -> Tree.node (List.sumBy totalSize sizes) sizes)
            (fun file -> Tree.leaf file.Size)
        |> Tree.bifold (fun acc size -> size :: acc) (fun acc _ -> acc) []

    fSolve totalSizes

let part1 totalSizes =
    totalSizes
    |> List.filter (fun totalSize -> totalSize <= 100_000)
    |> List.sum

let part2 totalSizes =
    let maxSize = List.max totalSizes
    let smallest =
        totalSizes
        |> Seq.sort
        |> Seq.skipWhile (fun size -> 70_000_000 - maxSize + size < 30_000_000)
        |> Seq.head

    smallest

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve part1 Environment.NewLine input
solve part2 Environment.NewLine input