(*
 │　Fig. 2の19個の正六角形の中に，1から19までの19個の数すべて
 │をダブりなく入れる。このとき，直線上に並んだ数(図中のA～O)の
 │和がどこも38になるようにしたい。何通りの配置が考えられるだろ
 │うか。全体を回転した解や，裏返した解は別の解とはしない。
*)

type MaybeBuilder() =
    member __.Bind(x, f) = match x with Some y -> f y | None -> None
    member __.Return x = Some x
let maybe = MaybeBuilder()

let solve() =
    seq { for a in 1 .. 14 do
          for c in a + 1 .. 18 do
          for h in c + 1 .. 19 do
          for l in a + 1 .. 19 do
          for q in a + 1 .. 19 do
          for s in a + 1 .. 19 do
          for e in 1 .. 19 -> a, c, e, h, l, q, s }
    |> Seq.choose (fun (a, c, e, h, l, q, s) ->
        let judge xs x = if 1 <= x && x <= 19 && List.forall ((<>) x) xs then Some(x::xs, x) else None
        maybe {
            let! xs, b = 38 - a - c |> judge [a; c; e; h; l; q; s]
            let! xs, d = 38 - a - h |> judge xs
            let! xs, g = 38 - c - l |> judge xs
            let! xs, m = 38 - h - q |> judge xs
            let! xs, p = 38 - l - s |> judge xs
            let! xs, r = 38 - q - s |> judge xs
            let! xs, f = 38 - d - e - g |> judge xs
            let! xs, k = 38 - b - f - p |> judge xs
            let! xs, o = 38 - g - k - r |> judge xs
            let! xs, n = 38 - m - o - p |> judge xs
            let! xs, i = 38 - d - n - r |> judge xs
            let! xs, j = 38 - h - i - k - l |> judge xs
            let! _ = if xs |> Seq.distinct |> Seq.length |> (=) 19 then Some () else None
            return (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) })
