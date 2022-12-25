open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./input.txt") |> Array.toList

let triplets =
    input
    |> List.map (fun line ->
        let valves =
            line
            |> Regex("[A-Z]{2}").Matches
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.Value)
            |> List.ofSeq

        let m = int (Regex("\d+").Match line).Value
        (List.head valves, (m, List.tail valves)))
    |> Map

let rec travel visited total cur =
    let (m, nexts) = Map.find cur triplets

    let notVisited = List.except visited nexts

    let acc = if m > 0 then [ (cur, total) ] else []

    let visits =
        match notVisited with
        | [] -> acc
        | _ ->
            notVisited
            |> List.map (fun next -> travel (cur :: visited) (total + 1) next)
            |> List.concat
            |> List.append acc

    visits
    |> List.groupBy fst
    |> List.map (snd >> (List.minBy snd))


let graph =
    triplets
    |> Map.map (fun k (m, _) -> m, travel [] 0 k)
    |> Map.filter (fun k (m, _) -> m > 0 || k = "AA")
    |> Map.map (fun k (m, vs) -> m, vs |> List.filter (fun (dest, _) -> dest <> k))

let rec traverse total steps maxSteps visited cur =
    let (rate, nexts) = Map.find cur graph

    let notVisited =
        List.filter (fun (dest, _) -> not (List.contains dest visited)) nexts

    let addedRate = rate * (max (maxSteps - steps) 0)

    if steps >= maxSteps || (List.isEmpty notVisited) then
        total + addedRate
    else
        let addedRate = rate * (maxSteps - steps)
        let openCost = if cur = "AA" then 0 else 1

        let openValve =
            notVisited
            |> List.map (fun (n, s) -> traverse (total + addedRate) (steps + s + openCost) maxSteps (cur :: visited) n)
            |> List.max

        openValve

let res1 = traverse 0 1 30 [] "AA"

printfn "%A" res1

let rec comb n l =
    match (n, l) with
    | (0, _) -> [ [] ]
    | (_, []) -> []
    | (n, x :: xs) ->
        let useX = List.map (fun l -> x :: l) (comb (n - 1) xs)
        let noX = comb n xs
        useX @ noX

let dests =
    Map.find "AA" graph
    |> fun (_, ls) -> List.map fst ls

let destlen = List.length dests

let splits = [ 3 .. (destlen / 2) ]

let all =
    splits
    |> List.map (fun n -> comb n dests)
    |> List.concat
    |> List.map (fun ls -> (ls, List.except ls dests))

let calc one other =
    async {
        let first = traverse 0 1 26 one "AA"
        let second = traverse 0 1 26 other "AA"
        return first + second
    }

let res2 =
    all
    |> List.map (fun (one, other) -> calc one other)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.max

printfn "%A" res2
