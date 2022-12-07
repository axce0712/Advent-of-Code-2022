open System
open System.IO

type Tree<'a, 'b> =
    | Node of 'a * Tree<'a, 'b> list
    | Leaf of 'b

type Item =
    | Directory of Name : string
    | File of Name : string * Size : int

type Command =
    | ChangeDirectory of string
    | List of Item list

let (|Int|_|) (input : string) =
    match Int32.TryParse input with
    | true, value -> Some value
    | false, _ -> None

let parseItem (item : string) =
    match item.Split(" ") with
    | [| "dir"; name |] -> Directory name
    | [| Int size; name |] -> File (name, size)
    | _ -> invalidArg (nameof item) "Invalid value"

let parseCommand lines =
    match lines with
    | "ls" :: rest ->
        let items = List.map parseItem rest
        List items
    | [ cmd ] ->
        match cmd.Split(" ") with
        | [| "cd"; dir |] -> ChangeDirectory dir
        | _ -> failwith $"Cannot handle cmd '%s{cmd}'"
    | cmd :: _ -> failwith $"Cannot2 handle cmd '%s{cmd}'"
    | [] -> invalidArg (nameof lines) "Lines must be present"

let parse (newLine : string) (input : string) =
    input.Split("$ ", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun part -> part.Trim().Split(newLine) |> Seq.toList |> parseCommand)
    |> Seq.toList

let rec getDirectories commands =
    let rec imp dirStack dirs commands =
        match commands with
        | [] -> dirs
        | ChangeDirectory ".." :: next -> imp (List.tail dirStack) dirs next
        | ChangeDirectory d :: next -> imp (d :: dirStack) dirs next
        | List items :: next ->
            let current = List.head dirStack
            let newDir = Map.add current items dirs
            imp dirStack newDir next

    imp [] Map.empty commands

let rec totalSize dirs = function
    | File (_, size) -> size
    | Directory name ->
        Map.find name dirs
        |> List.sumBy (totalSize dirs)

let solve newLine input =
    let commands = parse newLine input
    let dirs = getDirectories commands
    let sizePerDir =
        dirs
        |> Map.map (fun _ xs -> xs |> List.sumBy (totalSize dirs))

    let sum =
        sizePerDir
        |> Map.filter (fun _ totalSize -> totalSize <= 100_000)
        |> Map.values
        |> Seq.sum

    sum

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve Environment.NewLine input

solve "\n" @"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"