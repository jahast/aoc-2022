open System.IO
open System.Text.RegularExpressions
open System

let input = File.ReadAllLines("./input.txt") |> Array.toList

let times a b = a * b
let divisible a b = (b % a) = 0
let dupl f = (fun y -> f y y)

type Monkey =
    { WorryFunc: int -> int
      TestFunc: int -> bool
      TrueThrow: int
      FalseThrow: int }


let chunked =
    input
    |> List.filter ((<>) "")
    |> List.chunkBySize 6

let parseInt (str: string) =
    match Int32.TryParse str with
    | true, num -> Some(num)
    | _ -> None

let monkeys =
    chunked
    |> List.map (fun lines ->

        let coef = Regex("\d+").Match(lines[2]).Value |> parseInt

        let ff =
            if lines[ 2 ].Contains "+" then
                (+)
            else
                (times)

        let worryFunc =
            match coef with
            | None -> dupl ff
            | Some (num) -> ff num


        let testFunc =
            Regex("\d+").Match(lines[3]).Value
            |> int
            |> divisible

        let trueThrow = Regex("\d+").Match(lines[4]).Value |> int
        let falseThrow = Regex("\d+").Match(lines[5]).Value |> int

        { WorryFunc = worryFunc
          TestFunc = testFunc
          TrueThrow = trueThrow
          FalseThrow = falseThrow })

let items =
    chunked
    |> List.map (fun lines ->
        lines[1]
        |> Regex("\d+").Matches
        |> Seq.cast<Match>
        |> Seq.map (fun i -> int i.Value)
        |> List.ofSeq)
    |> List.mapi (fun i x -> (i, x))
    |> Map

let (>.) x f = (fun y -> f y x)

let states =
    [ 0..19 ]
    |> List.fold
        (fun state round ->
            [ 0 .. (monkeys.Length - 1) ]
            |> List.fold
                (fun (items, acc) idx ->

                    let itemsToDistribute = Map.find idx items
                    let monkey = monkeys[idx]

                    let newItems =
                        itemsToDistribute
                        |> List.map (monkey.WorryFunc)
                        |> List.map (3 >. (/))
                        |> List.fold
                            (fun its it ->
                                if (monkey.TestFunc it) then
                                    Map.change monkey.TrueThrow (Option.map ([ it ] >. (@))) its
                                else
                                    Map.change monkey.FalseThrow (Option.map ([ it ] >. (@))) its)
                            items
                        |> Map.add idx []

                    let newAcc = List.updateAt idx (itemsToDistribute.Length + (List.item idx acc)) acc

                    (newItems, newAcc)

                    )
                state)
        (items, (List.replicate (monkeys.Length) 0))

let res1 =
    snd states
    |> List.sortDescending
    |> List.take 2
    |> List.fold (fun acc x -> acc * x) 1

printfn "%A" res1

let divisors =
    chunked
    |> List.map (fun lines -> Regex("\d+").Match(lines[3]).Value |> int)

type ItemCounter = { Divisor: int; Remainder: int }

type Item = { Counters: ItemCounter list }

let complexItems =
    items
    |> Map.map (fun key ls ->
        ls
        |> List.map (fun it ->
            divisors
            |> List.map (fun div -> { Divisor = div; Remainder = it % div }))
        |> List.map (fun ls -> { Counters = ls }))

let applyWorry f (item: Item) =
    item.Counters
    |> List.map (fun counter -> { counter with Remainder = (f counter.Remainder) % counter.Divisor })
    |> (fun ls -> { Counters = ls })

let testItem idx (item: Item) =
    item.Counters
    |> List.item idx
    |> (fun i -> i.Remainder = 0)

// lazy
let states2 =
    [ 0..9999 ]
    |> List.fold
        (fun state round ->
            [ 0 .. (monkeys.Length - 1) ]
            |> List.fold
                (fun (items, acc) idx ->

                    let itemsToDistribute = Map.find idx items
                    let monkey = monkeys[idx]

                    let newItems =
                        itemsToDistribute
                        |> List.map (applyWorry monkey.WorryFunc)
                        |> List.fold
                            (fun its it ->
                                if (testItem idx it) then
                                    Map.change monkey.TrueThrow (Option.map ([ it ] >. (@))) its
                                else
                                    Map.change monkey.FalseThrow (Option.map ([ it ] >. (@))) its)
                            items
                        |> Map.add idx []

                    let newAcc =
                        List.updateAt
                            idx
                            (int64 itemsToDistribute.Length
                             + (List.item idx acc))
                            acc

                    (newItems, newAcc)

                    )
                state)
        (complexItems, (List.replicate (monkeys.Length) 0L))

let res2 =
    snd states2
    |> List.sortDescending
    |> List.take 2
    |> List.fold (fun acc x -> acc * x) 1L

printfn "%A" res2
