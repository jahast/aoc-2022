open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

let splat =
    input
    |> List.fold
        (fun grouped line ->
            match line with
            | "" -> [] :: grouped
            | _ -> (grouped.Head @ [ (int line) ]) :: grouped.Tail

            )
        [ [] ]

let sums = splat |> List.map List.sum

let res1 = sums |> List.max

printfn $"%A{res1}"

let res2 =
    sums
    |> List.sort
    |> List.rev
    |> List.take 3
    |> List.sum

printfn $"%A{res2}"
