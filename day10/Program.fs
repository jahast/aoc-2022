open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let states =
    input
    |> List.fold
        (fun acc ins ->
            let (loop, reg) = List.head acc

            match ins with
            | "noop" -> (loop + 1, reg) :: acc
            | Prefix "addx " regAdd ->
                [ (loop + 2, reg + (int regAdd))
                  (loop + 1, reg) ]
                @ acc
            | _ -> failwith ins)
        [ (1, 1) ]
    |> Map

let res1 =
    [ 20..40..220 ]
    |> Seq.map (fun l -> (Map.find l states) * l)
    |> Seq.sum

printfn "%A" res1

let res2 =
    [ 0..239 ]
    |> List.ofSeq
    |> List.map (fun i ->
        (Map.find (i + 1) states)
        |> (-) (i % 40)
        |> abs
        |> (>=) 1)
    |> List.map (fun b -> if b then "#" else ".")
    |> List.chunkBySize 40
    |> List.map (String.concat "")
    |> String.concat "\n"
    |> (fun s -> String.concat "" [ "\n"; s; "\n" ])

printfn "%A" res2
