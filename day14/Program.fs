open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./input.txt") |> Array.toList

let coorPairs =
    input
    |> List.map (fun line ->
        line
        |> Regex("\d+").Matches
        |> Seq.cast<Match>
        |> Seq.map (fun i -> int i.Value)
        |> List.ofSeq
        |> List.chunkBySize 2
        |> List.map (fun ls -> (ls[0], ls[1]))
        |> List.windowed 2
        |> List.map (fun ls -> (ls[0], ls[1])))

let range x xx =
    [ x; xx ]
    |> List.sort
    |> fun ls -> [ ls[0] .. ls[1] ]

let solids =
    coorPairs
    |> List.map (fun lineItems ->
        lineItems
        |> List.map (fun pair ->
            let (firstP, secondP) = pair
            let xs = range (fst firstP) (fst secondP)
            let ys = range (snd firstP) (snd secondP)
            let lazy1 = xs |> List.map (fun x -> (x, ys[0]))
            let lazy2 = ys |> List.map (fun y -> (xs[0], y))
            Set.union (set lazy1) (set lazy2))
        |> Set.unionMany)
    |> Set.unionMany

let plus (x, y) (xx, yy) = (x + xx, y + yy)

let dirs = [ (0, 1); (-1, 1); (1, 1) ]

let rec dropSand coord points maxY =

    if (snd coord) > maxY then
        None
    else
        let next =
            dirs
            |> List.map (plus coord)
            |> List.tryPick (fun c ->
                if (Set.contains c points) then
                    None
                else
                    Some c)

        match next with
        | None -> Some(Set.add coord points)
        | Some (c) -> dropSand c points maxY

let maxY = solids |> Set.map (snd) |> Seq.max

let res1 =
    solids
    |> List.unfold (fun points ->
        match dropSand (500, 0) points maxY with
        | None -> None
        | Some (ps) -> Some(1, ps))
    |> List.length

printfn "%A" res1

let rec dropSand2 coord points maxY =

    if (snd coord) = (maxY + 1) then
        Some(Set.add coord points)
    else
        let next =
            dirs
            |> List.map (plus coord)
            |> List.tryPick (fun c ->
                if (Set.contains c points) then
                    None
                else
                    Some c)

        match next with
        | None when coord = (500, 0) -> None
        | None -> Some(Set.add coord points)
        | Some (c) -> dropSand2 c points maxY

let res2 =
    solids
    |> List.unfold (fun points ->
        match dropSand2 (500, 0) points maxY with
        | None -> None
        | Some (ps) -> Some(1, ps))
    |> List.length
    |> (+) 1

printfn "%A" res2
