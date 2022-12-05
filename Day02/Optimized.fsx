open System.IO

let rounds =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> Array.map (fun s -> (int s[0] - int 'A', int s[2] - int 'X'))

let score1 (opp, you) = you + 1 + ((you - opp + 4) % 3) * 3

let score2 (opp, out) = ((opp + out + 2) % 3) + 1 + out * 3

printfn "%i" (Array.sumBy score1 rounds)
printfn "%i" (Array.sumBy score2 rounds)
