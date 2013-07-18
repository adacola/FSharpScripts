(*
http://kupc2013.contest.atcoder.jp/tasks/kupc2013_c
*)

/// x列の最上段のチョコレートを食べようとします。
/// 食べられる場合は、食べた結果の得点と残りのチョコレートの状態のSome。食べられない場合はNone。
let tryEat x (chocolates : int[][]) =
    let checkRange x = 0 <= x && x < chocolates.Length
    assert (checkRange x)
    let height x = chocolates.[x].Length
    let exists x y = checkRange x && height x > y
    // xの両隣の高さ(外側は高さ0とする)
    let neighborColumnLengths = [(if x = 0 then 0 else x - 1 |> height); (if x = chocolates.Length - 1 then 0 else x + 1 |> height)]
    let y = height x - 1
    // 両隣ともx列以上の高さの場合は食べられない
    if neighborColumnLengths |> List.forall ((<) y) then None else
    let neighbors = [x, y - 1; x - 1, y; x + 1, y] |> List.filter ((<||) exists)
    let score = chocolates.[x].[y]
    let chocolates' =
        chocolates |> Array.mapi (fun i column ->
            if i = x then column.[.. y - 1] else column
            |> Array.mapi (fun j taste -> if neighbors |> List.exists ((=) (i, j)) then 1 - taste else taste))
    Some(score, chocolates')

/// 甘いチョコレートを食べることのできる最大個数を計算します。
let calc (chocolates : int[][]) =
    let columnCount = chocolates.Length
    let endChocolates = Array.create columnCount [||]
    let rec next = function
        | [totalScore, chocolates] when chocolates = endChocolates -> totalScore
        | states -> 
            states |> List.collect (fun (totalScore, chocolates) ->
                [for i in 0 .. columnCount - 1 -> tryEat i chocolates] |> List.choose id
                |> List.map (fun (score, chocolates') -> totalScore + score, chocolates'))
            |> Seq.groupBy snd |> Seq.map (snd >> Seq.maxBy fst) |> Seq.toList
            |> next
    next [0, chocolates]

open System

let main() =
    let [|rowLength; columnLength|] = Console.ReadLine().Split([|' '|]) |> Array.map int
    let chocolates = Array.init columnLength (fun _ -> Array.create rowLength 0)
    for i in rowLength - 1 .. -1 .. 0 do
        let input = Console.ReadLine().Split([|' '|]) |> Array.map int
        for j in 0 .. columnLength - 1 do chocolates.[j].[i] <- input.[j]
    let result = calc chocolates
    printfn "%d" result
