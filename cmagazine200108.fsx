(*
Ｆｉｇ．５のように，１～１５の１５個の数
を環状に並べた。すると，隣り合っ
た２数の和がすべて平方数（ある数の
二乗になっている数）になった……。
と思ってよく見たら，１ヵ所平方数
になっていない（８＋９＝１７）。では，
「１～ＮのＮ個の数を全て１個ずつ使
って環状に並べ，隣り合う２数の和
がすべて平方数になる」ものができ
るのは，Ｎがいくつのときか。最小
のＮを求めよ。また，そのＮでの並
べ方は，回転・鏡像を除いて何通
りあるか。
*)
let solve() =
    let makeRing N =
        if N < 3 then invalidArg "N" "Nには3以上を指定してください。"
        /// 平方数のセット。
        let squareNumbers =
            Seq.initInfinite ((+) 2 >> fun i -> pown i 2) |> Seq.takeWhile ((>) (N * 2)) |> set
        /// ある数の隣に来ることのできる数のセットの辞書。
        let neighborDic =
            [for i in 1 .. N -> i, set [for j in 1 .. N do if i <> j && i + j |> squareNumbers.Contains then yield j]]
            |> Map.ofList
        /// リング平方数を作成する。
        let rec makeRing ((r::rs) as result) used =
            if result.Length = N then
                // リストがN個に達した場合、リストの最初と最後をつなげてリングにできるかどうかを判定し、リングにできるもののうち鏡像パターンを除く。
                let arr = result |> List.toArray
                if (neighborDic.[arr.[0]] |> Set.contains arr.[arr.Length - 1]) && (arr.[0] < arr.[arr.Length - 2])
                then [result] else []
            else
                // 隣に来ることのできる数のうち未使用の数をリストに追加して探索を進める。
                neighborDic.[r] - used |> Seq.collect (fun next -> makeRing <| next::r::rs <| Set.add next used)
                |> Seq.toList
        // 隣に来ることのできる数の個数が最も少ない数のうち最小の数から探索を開始する。
        // 初期値はNの値に従って一意となるため、回転パターンは発生しない。
        // なお、隣に来ることのできる数の個数が2未満のものが含まれている場合は、探索するまでもなく解なし。
        match neighborDic |> Map.toSeq |> Seq.minBy (snd >> Set.count) with
        | _, neighbors when neighbors.Count < 2 -> []
        | start, _ ->
            makeRing [start] (set [start])
    // リング平方数を作成できる最小のNと、その時の解のパターンを求める。
    Seq.initInfinite ((+) 3) |> Seq.pick (fun i -> i |> makeRing |> function [] -> None | result -> Some(i, result))
