open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./input.txt") |> Array.toList

let startingState =
    input
    |> List.takeWhile (fun line -> not (Regex(@"\d+").IsMatch(line)))
    |> List.map (Seq.toList)
    |> List.transpose
    |> List.map (fun line ->
        line
        |> List.filter Char.IsUpper
        |> List.map string)
    |> List.filter (fun l -> List.length l > 0)

let instructions =
    input
    |> List.skipWhile (fun line -> not (line.Contains "move"))
    |> List.map (fun line ->
        line
        |> Regex("\d+").Matches
        |> Seq.cast<Match>
        |> Seq.map (fun i -> int i.Value)
        |> List.ofSeq)
    |> List.map (fun ls -> (ls[0], ls[1] - 1, ls[2] - 1))

let endState inss f =
    inss
    |> List.fold
        (fun (state: string list list) (ins: (int * int * int)) ->
            let (count, fromStack, toStack) = ins

            let (toMove, toStay) = state[fromStack] |> List.splitAt count

            state
            |> List.updateAt fromStack toStay
            |> List.updateAt toStack ((toMove |> f) @ state[toStack]))
        startingState

let res1 =
    endState instructions List.rev
    |> List.map List.head
    |> String.concat ""

printfn $"%A{res1}"

let res2 =
    endState instructions id
    |> List.map List.head
    |> String.concat ""

printfn $"%A{res2}"
