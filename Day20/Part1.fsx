let indexMoves length moves =
    if moves < 0 then
        moves % length - 1
    else if moves > 0 then
        moves % (length - 1)
    else
        0
let nextIndex length index moves =
    let indexMoves = indexMoves length moves
    let increment =
        if index + indexMoves > length then 1 else 0

    (index + indexMoves + increment + length) % length

nextIndex 7 0 1
nextIndex 7 0 2
nextIndex 7 1 -3
nextIndex 7 2 3
nextIndex 7 2 -2
nextIndex 7 3 0
nextIndex 7 5 4

let adjustIndex length (oldIndex, newIndex) index =
    if index = oldIndex then
        newIndex
    else if newIndex > oldIndex && oldIndex < index && index <= newIndex then
        (index - 1 + length) % length
    else if oldIndex > newIndex && newIndex <= index && index < oldIndex then
        (index + 1 + length) % length
    else
        index

let mix encrypted =
    let indexes = Array.init (Array.length encrypted) id
    for index, moves in Array.indexed encrypted do
        let oldIndex = indexes[index]
        let newIndex =  nextIndex encrypted.Length oldIndex moves
        for i in 0 .. indexes.Length - 1 do
            let currentIndex = indexes[i]
            indexes[i] <- adjustIndex encrypted.Length (oldIndex, newIndex) currentIndex

    Array.permute (fun i -> indexes[i]) encrypted

mix [| 1; 2; -3; 3; -2; 0; 4 |]

let solve lines =
    let encrypted = Array.map int lines
    let decrypted = mix encrypted
    let zeroIndex = Array.findIndex ((=) 0) decrypted
    let grooveCoordinates =
        [ 1000; 2000; 3000 ]
        |> List.map (fun n -> Array.item ((zeroIndex + n) % decrypted.Length) decrypted)

    List.sum grooveCoordinates

solve [| "1" ;"2" ;"-3" ;"3" ;"-2" ;"0" ;"4" |]

open System.IO

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

#time
solve lines
#time