open System.IO
open System

let input = File.ReadAllLines("./input.txt") |> Array.toList

let parsed =
    input
    |> List.map (
        Seq.toList
        >> (List.map (fun c ->
            match c with
            | c when Char.IsUpper c -> (int >> (+) -64 >> (+) 26) c
            | c -> (int >> (+) -96) c))
    )

let res1 =
    parsed
    |> List.map (fun l ->
        l
        |> List.chunkBySize (l.Length / 2)
        |> List.map set
        |> Set.intersectMany)
    |> List.collect Set.toList
    |> List.sum

printfn $"%A{res1}"

let res2 =
    parsed
    |> List.chunkBySize 3
    |> List.map (Seq.map set >> Set.intersectMany)
    |> List.collect (Set.toList)
    |> List.sum

printfn $"%A{res2}"
