open System.IO
open System

let input =
    File.ReadAllLines("./input.txt")
    |> Array.toList
    |> List.head

let chars = input |> List.ofSeq |> List.map string

let solve win =
    chars
    |> List.windowed win
    |> List.findIndex (set >> Set.count >> (=) win)
    |> (+) win

let res1 = solve 4

printfn $"%A{res1}"

let res2 = solve 14

printfn $"%A{res2}"
