(*
http://kupc2013.contest.atcoder.jp/tasks/kupc2013_b
*)

let isValid (cages : int[]) (startIndex, endIndex, total) =
    cages.[startIndex - 1 .. endIndex - 1] |> Array.sum |> (=) total

let getAllPatterns cageCount maxLionCount =
    let rec getAllPatterns result = function
        | 0 -> result
        | cageCount ->
            let r = result |> Seq.collect (fun xs -> [for i in maxLionCount .. -1 .. 0 -> Array.append xs [|i|]])
            cageCount - 1 |> getAllPatterns r
    getAllPatterns [[||]] cageCount

let tryFind cageCount maxLionCount counts =
    getAllPatterns cageCount maxLionCount |> Seq.tryFind (fun cages -> counts |> List.forall (isValid cages))

open System

let main() =
    let [|cageCount; maxLionCount; counterCount|] = Console.ReadLine().Split([|' '|]) |> Array.map int
    let counts =
        [for _ in 1 .. counterCount ->
            let input = Console.ReadLine().Split([|' '|])
            int input.[0], int input.[1], int input.[2]]
    match tryFind cageCount maxLionCount counts with
    | Some result -> result |> Seq.map string |> String.concat " " |> printfn "%s"
    | None -> printfn "%d" -1
