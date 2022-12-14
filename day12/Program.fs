open System.IO
open System

let input = File.ReadAllLines("./test.txt") |> Array.toList

let matrLs = input |> List.map (List.ofSeq >> List.map int)

let (rows, cols) = (matrLs.Length, matrLs[0].Length)

let matrRaw =
    matrLs
    |> List.map List.indexed
    |> List.indexed
    |> List.collect (fun (x, ls) -> ls |> List.map (fun (y, it) -> ((x, y), it)))
    |> Map

let start =
    matrRaw
    |> Map.filter (fun _ v -> v = 83)
    |> Map.keys
    |> Seq.exactlyOne

let finish =
    matrRaw
    |> Map.filter (fun _ v -> v = 69)
    |> Map.keys
    |> Seq.exactlyOne

let matr =
    matrRaw
    |> Map.change start (fun _ -> Some(97))
    |> Map.change finish (fun _ -> Some(122))

let plus (x, y) (xx, yy) = (x + xx, y + yy)

let neighbourIdxs cur =
    [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
    |> List.map (plus cur)

let rec shortestPathNaive visited =
    let cur = List.head visited
    let curVal = Map.find cur matr

    // let newBests =
    //     bests
    //     |> Map.change cur (fun v ->
    //         let curLen = List.length visited

    //         match v with
    //         | None -> Some(curLen)
    //         | Some (num) -> Some(max num curLen))

    if cur = start then
        List.length visited
    else
        let validNeighbours =
            cur
            |> neighbourIdxs
            |> List.filter (fun k -> Map.containsKey k matr)
            |> List.filter (fun k -> not (List.contains k visited))
            |> List.filter (fun k -> Map.find k matr |> ((-) curVal) |> (<=) 1)

        if validNeighbours.Length = 0 then
            100000
        else
            validNeighbours
            |> List.map (fun k -> shortestPathNaive (k :: visited))
            |> List.min

let res1 = shortestPathNaive [ finish ]

printfn "%A" res1
