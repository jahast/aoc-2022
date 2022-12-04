open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

let parsed =
    input
    |> List.map (fun line ->
        line.Split ","
        |> Array.take 2
        |> Array.map (fun range ->
            range.Split "-"
            |> Array.map int
            |> (fun r -> (r[0], r[1])))
        |> (fun p -> (p[0], p[1]))

    )

let rangeLen tpl = (snd tpl) - (fst tpl)

let compare tpl other =
    (fst tpl) >= (fst other)
    && (snd tpl) <= (snd other)

let res1 =
    parsed
    |> List.map (fun pair ->
        let (smaller, bigger) =
            match pair with
            | (a, b) when (rangeLen a) < (rangeLen b) -> (a, b)
            | (a, b) -> (b, a)

        match compare smaller bigger with
        | true -> 1
        | _ -> 0)
    |> List.sum

printfn $"%A{res1}"

let overlaps first second =
    (fst first) <= (snd second)
    && (fst second) <= (snd first)

let res2 =
    parsed
    |> List.map (fun (a, b) ->
        match overlaps a b with
        | true -> 1
        | _ -> 0)
    |> List.sum

printfn $"%A{res2}"
