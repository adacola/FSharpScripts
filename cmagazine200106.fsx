(*
　3435という数は，１桁ずつに分け
て，それぞれの数を自分と同じ数で
べき乗して足し合わせると，元の数
である3435になる。
　3435 = 3^3 + 4^4 + 3^3 + 5^5
　このような数を“開き直り数”と呼
ぶことにする。ちょっと考えれば１
(=1^1)もこの種の数であることがわ
かる。では１以上の整数で，この１と
3435以外の開き直り数をすべて見つ
けていただきたい。ただし，ここで
は0^0は０とする。
*)

let solve() =
    /// 自分自身でべき乗した数を求める。なお、0 ^ 0 = 0 とする。
    let pow = function 0 -> 0u | n -> pown (uint32 n) n

    /// 探索の上限となる桁数。
    /// 開き直り数は高々10桁の数でしか存在しない。（11桁以上の場合、9^9を桁数分足しても元の数の桁数に満たないため）
    let limitOfDigit = 10

    /// 重複組み合わせを求める。
    let repeatedCombination n xs =
        let rec rc r = function
            | 0, _ | _, [] -> r |> Seq.singleton
            | n, xs ->
                xs |> Seq.unfold (function [] -> None | x::xs -> Some((x, x::xs), xs))
                |> Seq.collect (fun (x, xs) -> rc (x::r) (n - 1, xs))
        rc [] (n, xs)

    /// 指定された桁数の場合に出現する数字の最大のもの。
    let getMaxOfDigit =
        let digits =
            [for x in 0 .. 9 -> pow x |> string |> String.length, x] |> Seq.groupBy fst
            |> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Seq.max) |> Map.ofSeq
        fun n -> seq { n .. -1 .. 1 } |> Seq.pick (fun x -> Map.tryFind x digits)

    /// 指定された数列から開き直り数に変換する。開き直り数ではなかった場合はNone。
    let tryConvert xs =
        let digits = xs |> Seq.countBy id |> Map.ofSeq
        let num = xs |> Seq.sumBy pow
        if num |> string |> Seq.countBy (string >> int) |> Map.ofSeq |> (=) digits then Some num else None

    [for digit in 1 .. limitOfDigit do
        yield! repeatedCombination digit [0 .. getMaxOfDigit digit] |> Seq.choose tryConvert
                |> Seq.filter (fun r -> [0u; 1u; 3435u] |> List.forall ((<>) r))]
