open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

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

let manhattan (x, y) (xx, yy) = (abs (x - xx)) + (abs (y - yy))

let rec astar
    (frontier: (int * (int * int)) list)
    (cameFrom: Map<int * int, int * int>)
    (costSoFar: Map<int * int, int>)
    (goal: int * int)
    (matrix: Map<int * int, int>)
    =

    // just hack it
    if frontier.Length = 0 then
        None
    else
        let current = frontier |> List.minBy (fst) |> snd

        match current with
        | x when x = goal -> Some((Map.find current costSoFar, cameFrom))
        | _ ->
            let curVal = Map.find current matr

            let newFrontier, newCameFrom, newCostSoFar =
                (neighbourIdxs current)
                |> List.filter (fun k -> Map.containsKey k matrix)
                |> List.filter (fun k -> Map.find k matr |> ((-) curVal) |> (>=) 1)
                |> List.filter (fun coord ->
                    let newCost = (Map.find current costSoFar) + 1
                    let nextCost = Option.defaultValue 100000000 (Map.tryFind coord costSoFar)
                    newCost < nextCost)
                |> List.fold
                    (fun (frontier', cameFrom', costSoFar') coord ->
                        let newCost = (Map.find current costSoFar) + 1
                        let frontierCost = newCost + manhattan current goal
                        let newFrontier = (frontierCost, coord) :: frontier'
                        let newCameFrom = Map.add coord current cameFrom'
                        let newCostSoFar = Map.add coord newCost costSoFar'
                        (newFrontier, newCameFrom, newCostSoFar))
                    (frontier, cameFrom, costSoFar)

            let currentRemoved =
                newFrontier
                |> List.filter (fun x -> snd x |> (<>) current)

            astar currentRemoved newCameFrom newCostSoFar goal matrix

let traversed = astar [ (0, finish) ] Map.empty (Map [ finish, 0 ]) start matr

let res1 = traversed |> Option.get |> fst

printfn "%A" res1

let res2 =
    matr
    |> Map.filter (fun k v -> v = 97)
    |> Map.keys
    |> Seq.map (fun k -> astar [ (0, finish) ] Map.empty (Map [ finish, 0 ]) k matr)
    |> Seq.choose (Option.map fst)
    |> Seq.min

printfn "%A" res2
