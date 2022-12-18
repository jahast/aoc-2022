open System.IO
open System

type Packet =
    | Packet of Packet list
    | Value of int

let input = File.ReadAllLines("./input.txt") |> Array.toList

let split (str: string list) =
    str
    |> String.concat ""
    |> (fun s -> s.Split ",")
    |> Seq.map (fun s -> s |> List.ofSeq |> Seq.map string |> List.ofSeq)
    |> List.ofSeq

let isNum (str: string list) =
    not (
        str |> List.contains "["
        || str |> List.contains "]"
        || str |> List.contains ","
    )

let Init (list: List<_>) = list[.. (list.Length - 2)]

let findMatching (str: string list) (startIdx: int) =
    str
    |> List.indexed
    |> List.skip startIdx
    |> List.fold
        (fun (depth, res) (i, s) ->
            if (Option.isSome res) then
                (depth, res)
            else
                match s with
                | "[" -> (depth + 1, res)
                | "]" when depth = 0 -> (depth, Some(i))
                | "]" -> (depth - 1, res)
                | _ -> (depth, res))
        (-1, None)
    |> snd

let rec parse (str: string list) : Packet list =
    let mutable idx = 0
    let mutable acc: Packet list = []

    while idx < (str.Length) && str.Length > 0 do
        (match (str.Item idx) with
         | "[" ->
             match (findMatching str idx) with
             | Some (j) ->
                 let inner =
                     str
                     |> (fun ls -> ls[(idx + 1) .. (j - 1)])
                     |> parse

                 acc <- acc @ [ Packet(inner) ]
                 idx <- j + 1
             | _ -> failwithf "%A" str
         | _ when isNum str[idx .. (idx + 1)] ->
             acc <-
                 acc
                 @ [ Value(str[idx .. (idx + 1)] |> String.concat "" |> int) ]

             idx <- idx + 2
         | _ when isNum str[idx..idx] ->
             acc <-
                 acc
                 @ [ Value(str[idx..idx] |> String.concat "" |> int) ]

             idx <- idx + 1

         | _ -> idx <- idx + 1)
        |> ignore

    acc

let pairs =
    input
    |> List.filter ((<>) "")
    |> List.map (List.ofSeq >> List.map string)
    |> List.map (parse)
    |> List.chunkBySize 2
    |> List.map (fun ls -> (ls[0][0], ls[1][0]))
    |> List.indexed

let rec compare (left: Packet option) (right: Packet option) =
    match (left, right) with
    | (Some (Value (l)), Some (Value (r))) ->
        if l < r then Some(true)
        elif l > r then (Some(false))
        else None
    | (Some _, None) -> Some false
    | (None, Some (_)) -> Some true
    | (Some (Value (l)), (Some (Packet (r)))) -> compare (Some(Packet([ Value(l) ]))) (Some(Packet(r)))
    | (Some (Packet (l)), Some (Value (r))) -> compare (Some(Packet(l))) (Some(Packet([ Value(r) ])))
    | (Some (Packet (l)), Some (Packet (r))) ->
        let maxLen = max l.Length r.Length

        [ 0 .. (maxLen - 1) ]
        |> List.tryPick (fun i -> compare (List.tryItem i l) (List.tryItem i r))
    | _ -> failwithf "%A" (left, right)

let res1 =
    pairs
    |> List.map (fun (i, (l, r)) ->
        if (compare (Some(l)) (Some(r))).Value then
            i + 1
        else
            0)
    |> List.sum

printfn "%A" res1

let packets =
    input
    |> List.filter ((<>) "")
    |> List.map (List.ofSeq >> List.map string)
    |> List.map (parse)
    |> List.map (fun p -> p[0])
    |> List.append [ Packet([ Packet([ Value(2) ]) ]) ]
    |> List.append [ Packet([ Packet([ Value(6) ]) ]) ]

let sorted =
    packets
    |> List.sortWith (fun l r ->
        if (compare (Some l) (Some r)).Value then
            -1
        else
            1)

let idx el ls = ls |> List.findIndex ((=) el) |> (+) 1

let res2 =
    (idx (Packet [ Packet [ Value 2 ] ]) sorted)
    * (idx (Packet [ Packet [ Value 6 ] ]) sorted)

printfn "%A" res2
