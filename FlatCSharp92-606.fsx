open System
open System.IO

type SubFolder = { Name : string }
type Folder = { Name : string; SubFolders : SubFolder seq }

let getFolders (text : string) =
    let isSubFolder line = String.length line >= 2 && line.[.. 1] = ".."
    text.Split([|"\r\n"; "\r"; "\n"|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toSeq |> Seq.unfold (function
    | lines when Seq.isEmpty lines -> None
    | lines ->
        let folder, rest = Seq.head lines, Seq.skip 1 lines
        if isSubFolder folder then failwith "最初にサブフォルダが来たので不正です"
        let subFolders, rest = Seq.takeWhile isSubFolder rest, Seq.skipWhile isSubFolder rest
        Some({ Name = folder; SubFolders = subFolders |> Seq.map (fun name -> { Name = name }) },
             rest))

let folders =
    "フォルダ1
..サブフォルダ1-1
..サブフォルダ1-2
フォルダ2
フォルダ3
..サブフォルダ3-1
フォルダ4" |> getFolders
