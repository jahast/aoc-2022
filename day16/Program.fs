open System.IO
open System.Text.RegularExpressions


let input = File.ReadAllLines("./test.txt") |> Array.toList

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

    visits
    |> List.groupBy fst
    |> List.map (snd >> (List.minBy snd))


let graph =
    triplets
    |> Map.map (fun k (m, _) -> m, travel [] 0 k)


let rec traverse total steps maxSteps visited cur =
    let (rate, nexts) = Map.find cur graph

    let notVisited =
        nexts
        |> set
        |> fun s -> Set.difference s visited

    if steps = maxSteps || notVisited.Count = 0 then
        total
    else
        let noOpen =
            notVisited
            |> Set.map (traverse total (steps + 1) maxSteps (Set.add cur visited))
            |> Set.maxElement

        if rate > 0 then
            let addedRate = rate * (maxSteps - steps + 1)

            let openValve =
                notVisited
                |> Set.map (traverse (total + addedRate) (steps + 2) maxSteps (Set.add cur visited))
                |> Set.maxElement

            max openValve noOpen
        else
            noOpen

let res1 = traverse 0 1 30 Set.empty "AA"

printfn "%A" res1
