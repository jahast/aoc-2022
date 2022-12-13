open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines("./input.txt") |> Array.toList

let times a b = a * b
let divisible a b = (a % b) = 0

let chunked =
    input
    |> List.filter ((<>) "")
    |> List.chunkBySize 6

let monkeys =
    chunked
    |> List.map (fun lines ->
        let coef = Regex("\d+").Match(lines[2]).Value |> int

        let worryFunc =
            if lines[ 2 ].Contains "+" then
                (+) coef
            else
                times coef

        let testFunc =
            Regex("\d+").Match(lines[3]).Value
            |> int
            |> divisible

        let trueThrow = Regex("\d+").Match(lines[4]).Value |> int
        let falseThrow = Regex("\d+").Match(lines[5]).Value |> int

        (worryFunc, testFunc, trueThrow, falseThrow))

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


let states =
    [ 0..19 ]
    |> List.fold
        (fun is _ ->
            [ 0 .. (monkeys.Length) ]
            |> List.fold
                (fun items idx ->
                    match idx with
                    | _ -> items)
                is)
        items
