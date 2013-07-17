(*
http://kupc2013.contest.atcoder.jp/tasks/kupc2013_a
*)

let initial = 1, "kogakubu10gokan"

let getName year = Seq.sort >> Seq.takeWhile (fst >> (>=) year) >> Seq.last >> snd

open System

let main() =
    let N, Q = let input = Console.ReadLine().Split([|' '|]) in int input.[0], int input.[1]
    let names = initial::[for _ in 1 .. N -> let input = Console.ReadLine().Split([|' '|]) in int input.[0], input.[1]]
    getName Q names |> printfn "%s"
