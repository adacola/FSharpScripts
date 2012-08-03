(*
# 「分母と分子にある同じ数字を消す」という間違った約分を行うと、
# 正しく約分した値と同じになる分数が存在する。
# （例えば「187/748」は、普通に約分すると1/4に、分母と分子で重複している7と8を消すと1/4となる）
# このような分数は無限に存在するので、下のような条件を付ける。
#
# (1)分母と分子で同じ数字があればそのペアは必ず消す
# (2)消されるペアは、分母と分子に１文字ずつしか入っていない。つまり消すペアは一意に決まり、
# また、同じ数字で複数のペアが存在することもない
# (3)最終的に分母分子がそれぞれ１桁でできた、１未満の既約分数となる
# (4)数字の０（ゼロ）は使わない
# (5)負の分数は考えない
#
# この条件で、上記の間違った約分を行った結果が、正しく約分した値と同じになる分数は
# 何通りあるか。
*)

let solve() =
    let permutation k ls =
        let rec p a b = function
            | _, [] | 0, _ -> a::b
            | k, ls -> List.fold (fun x y -> p (y::a) x (k - 1, List.filter ((<>) y) ls)) b ls
        p [] [] (k, ls)
    let rec gcd = function x, 0 -> x | x, y -> gcd (y, x % y)
    let add x xs =
        seq { for i in 0 .. List.length xs ->
                [Seq.take i xs; Seq.singleton x; Seq.skip i xs] |> Seq.concat |> Seq.toList }
    let toNumbers x = add x >> Seq.map (List.reduce (fun x y -> x * 10 + y))
    let toDigits = Seq.unfold (function 0 -> None | x -> Some(x % 10, x / 10)) >> Seq.sort >> Seq.toList
    let digits = set [1 .. 9]
    [for d in 2 .. 9 do
     for n in 1 .. d - 1 do
        if gcd (d, n) = 1 then yield! [for k in 1 .. 7 -> (n, d, k)]]
    |> Seq.collect (fun (n, d, k) ->
        digits |> Set.remove n |> Set.remove d |> Set.toList |> permutation k
        |> Seq.collect (fun ps ->
            let ds = d::ps |> List.sort
            ps |> toNumbers n |> Seq.choose (fun nf -> if nf % n = 0 then Some(nf, nf / n * d) else None)
            |> Seq.filter (fun (nf, df) -> let g = gcd (df, nf) in nf / g = n && df / g = d && toDigits df = ds)))

solve() |> Seq.length
