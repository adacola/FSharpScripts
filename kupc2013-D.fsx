(*
http://kupc2013.contest.atcoder.jp/tasks/kupc2013_d

各列のy座標が最も大きい地点からできるだけ横に広い長方形のカーペットを設置する。
各x座標について以下のステップを行う。
1. カーペット情報(各カーペットはy座標の値のSet)を参照する
2. 現在x座標に対応する最大y座標より大きいカーペットを削除する
3. 最大y座標と等しいカーペットがある場合、x+1に移行
4. すべてのカーペットが最大y座標より小さい場合、そのy座標を持つカーペットを追加し、総カーペット数を加算して、x+1に移行
*)

open System

let proc (ys : int[]) =
    let rec proc carpets result = function
        | x when x = ys.Length -> result
        | x ->
            let y = ys.[x]
            match carpets with
            | c::cs when c > y -> proc cs result x
            | c::cs when c = y -> proc (c::cs) result (x + 1)
            | cs -> proc (y::cs) (result + 1) (x + 1)
    proc [] 0 0

open System

let main() =
    Console.ReadLine() |> ignore
    let ys = Console.ReadLine().Split([|' '|]) |> Array.map int
    let result = proc ys
    printfn "%d" result

main()
