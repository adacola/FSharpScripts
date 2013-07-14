(*
Meertens数とは、
Gödel数（自然数の各桁を各素因数の指数においた数。例えば123のGödel数は、2^1 * 3^2 * 5^3 = 2250）が自身と等しい数のことである。
最小のMeertens数を高速に求めるプログラムを作成せよ。

実装方針：
自然数 x のGödel数を求める関数をg(x)とする。この関数について以下の性質がある。
性質1. g(102) = g(1020) = g(10200) = … のように、x の桁の最後の0は無視してもg(x)の結果は変わらない。
性質2. y = g(x) とすると、y の桁の最後につく0の個数（100の場合は2個）は、x の上から1桁目と3桁目の数字の小さい方となる。（2 * 5 = 10 が因数に含まれる分だけ桁の最後に0がつくため）

上記のことから、以下のようにすることで探索量を削減できる。
1. 性質1から、桁の最後に0がつく数は探索の対象外とする。
2. それ以外の数については、その数の末尾に性質2より求められる個数の0を付加した数について、Meertens数かどうかを判断する。
*)

let getMeertens() =
    let primes n =
        let oddPrimes =
            (3L, []) |> Seq.unfold (fun (x, ps) ->
                if ps |> List.forall ((%) x >> (<>) 0L) then Some(Some(x), (x + 2L, x::ps)) else Some(None, (x + 2L, ps)))
        Seq.singleton (Some 2L) |> Seq.append <| oddPrimes
        |> Seq.choose id |> Seq.take n |> Seq.toList

    let appendZeros xs =
        match xs with
        | two::_::five::_ ->
            let zeroCount = min two five
            xs @ (List.replicate zeroCount 0)
        | _ -> xs

    let toDigits = Seq.unfold (function 0 -> None | n -> Some(n % 10, n / 10)) >> Seq.toList >> List.rev

    let countUp n = n + if n % 10 = 9 then 2 else 1

    let tryGetMeertens n =
        let xs = n |> toDigits |> appendZeros
        let gödel =
            primes xs.Length |> Seq.zip <| xs
            |> Seq.map ((<||) pown) |> Seq.reduce (*)
        let number = xs |> Seq.map int64 |> Seq.reduce (fun x y -> x * 10L + y)
        if gödel = number then Some gödel else None

    1 |> Seq.unfold (fun n -> Some(tryGetMeertens n, countUp n))
    |> Seq.pick id

getMeertens()
