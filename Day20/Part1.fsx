let swap arr sourceIndex targetIndex =
    let item = Array.item sourceIndex arr
    Array.set arr sourceIndex (Array.item targetIndex arr)
    Array.set arr targetIndex item

let adjust indexMap length index =
    (indexMap index + length) % length

let mix (encryptedFile : int array) : int array =
    let indexes = Array.init encryptedFile.Length id
    let lastIndex = encryptedFile.Length - 1
    let mutable offset = 0
    for index in 0 .. lastIndex do
        let moves = Array.item index encryptedFile
        let indexMap =
            if moves > 0 then fun x -> x + 1
            else if moves < 0 then fun x -> x - 1
            else id

        for _ in 1 .. abs moves do
            let actual = indexes[index]
            let next = adjust indexMap encryptedFile.Length actual
            let nextIndex = Array.findIndex ((=) next) indexes
            swap indexes index nextIndex
            offset <-
                match actual, next with
                | 0, idx when idx = lastIndex -> offset - 1
                | idx, 0 when idx = lastIndex -> offset + 1
                | _ -> offset

    encryptedFile
    |> Array.permute (adjust (fun i -> indexes[i] + offset) encryptedFile.Length)

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