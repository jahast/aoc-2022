open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./input.txt") |> Array.toList

let manhattan (x, y) (xx, yy) = abs (xx - x) + abs (yy - y)

let triplets =
    input
    |> List.map (fun line ->
        line
        |> Regex("-?\d+").Matches
        |> Seq.cast<Match>
        |> Seq.map (fun i -> int64 i.Value)
        |> List.ofSeq
        |> fun ls -> (ls[0], ls[1]), (ls[2], ls[3])
        |> fun (s, b) -> (s, b, manhattan s b))

let minX =
    triplets
    |> List.map (fun (s, _, m) -> (fst s) - m)
    |> List.min

let maxX =
    triplets
    |> List.map (fun (s, _, m) -> (fst s) + m)
    |> List.max

let yPos = 2000000L

let beaconsOnY =
    triplets
    |> List.map (fun (_, b, _) -> b)
    |> List.distinct
    |> List.sumBy (fun (_, y) ->
        y
        |> (=) yPos
        |> function
            | true -> 1
            | false -> 0)

let covered =
    [ minX..maxX ]
    |> List.sumBy (fun x ->
        triplets
        |> List.exists (fun (s, _, m) -> (manhattan (x, yPos) s) <= m)
        |> function
            | true -> 1
            | false -> 0)

printfn "%A" (covered - beaconsOnY)

let toLines (x, y) m =
    [ (1L, (y + m + 1L) - x)
      (-1L, (y + m + 1L) + x)
      (1L, (y - m - 1L) - x)
      (-1L, (y - m - 1L) + x) ]

let (asc, desc) =
    triplets
    |> List.map (fun (s, _, m) -> toLines s m)
    |> List.concat
    |> List.distinct
    |> List.partition (fun (k, _) -> if k = 1L then true else false)

let intersections =
    asc
    |> List.allPairs desc
    |> List.map (fun (one, other) ->
        let x =
            ((snd other) - (snd one))
            / ((fst one) - (fst other))

        let y = (fst one) * x + (snd one)
        (x, y))

let found =
    intersections
    |> List.distinct
    |> List.filter (fun (x, y) ->
        0L <= x
        && x <= 4000000L
        && 0L <= y
        && y <= 4000000L)
    |> List.find (fun c ->
        triplets
        |> List.forall (fun (s, _, m) -> manhattan s c > m))

let res2 = (4000000L * (fst found)) + (snd found)

printfn "%A" res2
