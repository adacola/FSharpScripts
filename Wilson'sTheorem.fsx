(*
【お題】
ウィルソンの定理を使って素数を判定する関数is_primeを実装し、
10000以下の素数以外の数の和を求めよ

ウィルソンの定理
pが素数 <=> (p-1)!+1 (mod p) == 0
*)

open System.Collections.Generic

let factorial =
    let memo = Dictionary()
    let rec factorial result (n : int) =
        match n, memo.TryGetValue n with
        | 0, _ | 1, _ -> result
        | _, (true, r) -> r * result
        | _, _ -> factorial (result * bigint(n)) (n - 1)
    fun n ->
        let result = factorial 1I n
        memo.[n] <- result
        result

let isPrime = function
    | p when p < 1 -> invalidArg "n" "正の整数を指定してください。"
    | 1 -> false
    | p -> (factorial (p - 1) + 1I) % (bigint(p)) = 0I

seq { 1 .. 10000 } |> Seq.filter (isPrime >> not) |> Seq.sum
