open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

let parsed =
    input
    |> List.map (fun line ->
        match line.Split " " |> Array.toList with
        | [ first; second ] -> (first, second)
        | _ -> failwithf ""

    )

let toPoints mark =
    match mark with
    | "X" -> 1
    | "Y" -> 2
    | "Z" -> 3
    | inp -> failwith inp

let winOrLose tpl =
    match tpl with
    | ("A", "X") -> 3
    | ("B", "Y") -> 3
    | ("C", "Z") -> 3
    | ("A", "Y") -> 6
    | ("B", "Z") -> 6
    | ("C", "X") -> 6
    | _ -> 0

let res1 =
    parsed
    |> List.map (fun tpl -> winOrLose tpl + (tpl |> snd |> toPoints))
    |> List.sum

printfn $"%A{res1}"

let toNum mark =
    match mark with
    | "X" -> 1
    | "A" -> 1
    | "Y" -> 2
    | "B" -> 2
    | "C" -> 3
    | "Z" -> 3
    | inp -> failwith inp

let ownPoints tpl =
    let (other, roundRes) = tpl

    match roundRes with
    | 1 -> (other + 1) % 3 + 1
    | 2 -> other
    | 3 -> (other) % 3 + 1
    | _ -> failwith ""

let winOrLosePoints num =
    match num with
    | 1 -> 0
    | 2 -> 3
    | 3 -> 6
    | _ -> failwith ""

let res2 =
    parsed
    |> List.map (fun tpl -> ((fst >> toNum) tpl, (snd >> toNum) tpl))
    |> List.map (fun tpl -> (ownPoints tpl) + (tpl |> snd |> winOrLosePoints))
    |> List.sum

printfn $"%A{res2}"
