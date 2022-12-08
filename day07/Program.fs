open System.IO

type Folder =
    { Files: (string * int64) list
      Children: string list }

let input = File.ReadAllLines("./input.txt") |> Array.toList

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let Init (list: List<_>) = list[.. (list.Length - 2)]

let back (path: string) =
    path.Split "/"
    |> List.ofArray
    |> Init
    |> String.concat "/"

let path (current: string) (next: string) =
    if current.EndsWith "/" then
        current + next
    else
        String.concat "/" [ current; next ]

let rec toFolders (lines: string list) (current: string) (acc: Map<string, Folder>) =
    match (List.tryHead lines) with
    | None -> acc
    | Some ("$ cd ..") -> toFolders (List.skip 1 lines) (back current) acc
    | Some (Prefix "$ cd " fileName) -> toFolders (List.skip 1 lines) (path current fileName) acc
    | Some ("$ ls") ->
        let lsLines =
            lines
            |> List.skip 1
            |> List.takeWhile (fun l -> not (l.StartsWith "$"))

        let (childrenLines, fileLines) =
            lsLines
            |> List.partition (fun l -> l.StartsWith "dir")

        let files =
            fileLines
            |> List.map (fun l -> l.Split " " |> (fun ls -> (ls[1], int64 ls[0])))

        let children =
            childrenLines
            |> List.map (fun l -> l.Split " " |> (fun ls -> path current ls[1]))

        let newAcc =
            acc
            |> Map.add current { Files = files; Children = children }

        let toSkip = lsLines |> List.length |> (+) 1

        toFolders (lines |> List.skip toSkip) current newAcc

    | Some (x) -> failwith x

let folders = toFolders (input |> List.skip 1) "/" Map.empty

let rec findSizes current =
    let currentFolder = Map.find current folders
    let fileSizeSum = currentFolder.Files |> List.sumBy (fun f -> snd f)

    let childrenSizes =
        currentFolder.Children
        |> List.map (fun c -> findSizes c)

    let folderSizes = childrenSizes |> List.collect fst
    let accumulation = childrenSizes |> List.sumBy snd

    let total = fileSizeSum + accumulation

    (total :: folderSizes, total)

let fileSizes = findSizes "/"

let res1 =
    fileSizes
    |> fst
    |> List.filter ((>=) 100000L)
    |> List.sum

printfn "%A" res1

let totalSpaceUsed = fileSizes |> snd
let freeSpaceNow = 70000000L - totalSpaceUsed
let freeSpaceNeeded = 30000000L - freeSpaceNow

let res2 =
    fileSizes
    |> fst
    |> List.filter ((<=) freeSpaceNeeded)
    |> List.min

printfn "%A" (res2)
