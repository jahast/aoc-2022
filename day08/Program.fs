open System.IO

let input = File.ReadAllLines("./input.txt") |> Array.toList

let matr =
    input
    |> List.map (List.ofSeq >> (List.map (string >> int)))

let visible (row: int list) =
    let withIndex = row |> List.mapi (fun i e -> (i, e))

    let forwardVisible =
        withIndex
        |> List.fold
            (fun (highest, visible) (idx, cur) ->
                if cur > highest then
                    (cur, idx :: visible)
                else
                    (highest, visible))
            (-1, [])
        |> snd

    let backwardVisible =
        (-1, [])
        |> List.foldBack
            (fun (idx, cur) (highest, visible) ->
                if cur > highest then
                    (cur, idx :: visible)
                else
                    (highest, visible))
            withIndex
        |> snd

    set forwardVisible
    |> Set.union (set backwardVisible)
    |> Set.toList

let horizontalVisible =
    matr
    |> List.mapi (fun i row -> visible row |> List.map (fun vIdx -> (i, vIdx)))
    |> List.collect id

let verticalVisible =
    matr
    |> List.transpose
    |> List.mapi (fun j col -> visible col |> List.map (fun vIdx -> (vIdx, j)))
    |> List.collect id

let res1 =
    set horizontalVisible
    |> Set.union (set verticalVisible)
    |> Set.count

printfn "%A" res1

let takeUntil f ls =
    ls
    |> List.fold
        (fun (arr, stopped) item ->
            if stopped then
                (arr, stopped)
            else
                let stopNow = not (f item)
                (arr @ [ item ], stopNow))
        ([], false)
    |> fst

let nVisible (h: int) (arr: int list) =
    arr |> takeUntil ((>) h) |> List.length |> max 1

let rowMultiplier (row: int list) (idx: int) =
    let (before, rest) = row |> List.splitAt idx
    let (tree, after) = (rest.Head, rest.Tail)

    (before |> List.rev |> nVisible tree)
    * (after |> nVisible tree)

let transposed = matr |> List.transpose

let res2 =
    [ 1 .. (matr.Length - 2) ]
    |> List.allPairs [ 1 .. (matr[0].Length - 2) ]
    |> List.map (fun (i, j) ->
        let rowMult = rowMultiplier (matr[i]) j
        let columnMult = rowMultiplier (transposed[j]) i
        rowMult * columnMult)
    |> List.max

printfn "%A" res2
