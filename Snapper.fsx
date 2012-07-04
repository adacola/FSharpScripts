open System
open System.IO

let solveSnapper N K = (K + 1) % (pown 2 N) = 0

let input fileName =
    fileName |> File.ReadLines |> Seq.skip 1
    |> Seq.map (fun line ->
        line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        |> function [|N; K|] -> int N, int K | _ -> failwith "入力エラー")

let output fileName =
    Seq.mapi (fun i result -> sprintf "Case #%d: %s" (i + 1) (if result then "ON" else "OFF"))
    >> fun out -> File.WriteAllLines(fileName, out)

input @"C:\Users\Wasabi\Documents\A-large-practice.in"
|> Seq.map ((<||) solveSnapper) |> output @"C:\Users\Wasabi\Documents\A-large-practice.out"
