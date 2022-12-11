open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

let ins =
    input
    |> List.map (fun l ->
        let splat = (l.Split " ")
        (splat[0], int (splat[1])))
    |> List.map (fun (dir, count) -> dir |> List.replicate count)
    |> List.collect id


let offsets =
    Map [ "R", (1, 0)
          "L", (-1, 0)
          "U", (0, 1)
          "D", (0, -1) ]

let plus (ax, ay) (bx, by) = (ax + bx, ay + by)
let minus (ax, ay) (bx, by) = (ax - bx, ay - by)

let newTail tail head =
    let diff as (dx, dy) = minus head tail

    let tailOffset =
        match diff with
        | (2, 0)
        | (-2, 0) -> (dx / 2, 0)
        | (0, 2)
        | (0, -2) -> (0, dy / 2)
        | (a, b) when abs (a) >= 2 || abs (b) >= 2 -> (a / abs (a), b / abs (b))
        | _ -> (0, 0)

    plus tail tailOffset

let rec traverse h t visited ins =
    match (List.tryHead ins) with
    | None -> visited
    | Some (dir) ->
        let offs = Map.find dir offsets
        let newH = plus h offs

        let newT = newTail t newH
        let newVisited = visited |> Set.add newT

        traverse newH newT newVisited (List.tail ins)

let res1 =
    ins
    |> traverse (0, 0) (0, 0) Set.empty
    |> Set.count

printfn "%A" res1

let rec traverse2 head tails visited ins =
    match (List.tryHead ins) with
    | None -> visited
    | Some (dir) ->
        let offset = Map.find dir offsets
        let newHead = plus head offset

        let newTails =
            tails
            |> List.fold
                (fun ls t ->
                    let tailPart = List.last ls |> newTail t
                    ls @ [ tailPart ])
                [ newHead ]
            |> List.tail

        let newVisited = visited |> Set.add (List.last newTails)

        traverse2 newHead newTails newVisited (List.tail ins)

let res2 =
    ins
    |> traverse2 (0, 0) (List.replicate 9 (0, 0)) Set.empty
    |> Set.count

printfn "%A" res2
