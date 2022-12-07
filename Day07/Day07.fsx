open System
open System.IO

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

let rec totalSize dirStack dirs = function
    | File (_, size) -> size
    | Directory name ->
        let newDirStack = name :: dirStack

        dirs
        |> Map.find newDirStack
        |> List.sumBy (totalSize newDirStack dirs)

let solve fSolve newLine input =
    let dirs = input |> split newLine |> List.map words |> parse
    let sizePerDir =
        dirs
        |> Map.map (fun d xs -> xs |> List.sumBy (totalSize d dirs))

    fSolve sizePerDir

let part1 dirs =
    dirs
    |> Map.filter (fun _ totalSize -> totalSize <= 100_000)
    |> Map.values
    |> Seq.sum

let part2 dirs =
    let sizes = dirs |> Map.values |> Seq.toList
    let maxSize = List.max sizes
    let smallest =
        sizes
        |> Seq.sort
        |> Seq.skipWhile (fun size -> 70_000_000 - maxSize + size < 30_000_000)
        |> Seq.head

    smallest

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve part1 Environment.NewLine input
solve part2 Environment.NewLine input