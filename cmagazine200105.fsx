(*
Fig. 1のように，星型が描かれた盤がある。その頂点と交点には
黒丸が描かれており，この10か所の黒丸のうち，9か所に10円玉
を表向きにして置く（最も上の点以外）。これをスタートとして，以下のル
ールで10円玉を移動していく。
　1.1か所空いた場所(黒丸)に移動できる10円玉は，ほかの1枚あ
　　るいは2枚 の10円玉を跳び越してくるもののみ
　2.跳び越しは線に沿って直線状に行われ，曲がり跳びは許され
　　ない
　3.跳び越された10円玉は裏返される(表向きだったものは裏向
　　きに，裏向きだったものは表向きになる)。跳び越した10円
　　玉の向きは，そのまま
　さて，上記の操作を繰り返してすべての10円玉を裏向きにする
　には，1回の10円玉の移動を1手と数えるとすると，最低何手必
　要だろうか。もちろん，10円玉は黒丸以外の場所に移動できな
　いし，1か所に2枚以上重ねることもできない。

Fig.1
　　 ●

●　●  ●　●
　●  　●
　　 ●
●　    ●
*)
open System.Collections.Generic

let solve() =
    /// 移動可能なマスおよび移動の際に反転する全マス。
    let movableAndReversedCoins =
        [|
            dict [ 5, [2]; 6, [3]; 8, [2; 5]; 9, [3; 6] ]
            dict [ 3, [2]; 4, [2; 3]; 7, [5]; 9, [5; 7] ]
            dict [ 4, [3]; 8, [5] ]
            dict [ 1, [2]; 9, [6] ]
            dict [ 1, [2; 3]; 2, [3]; 7, [6]; 8, [6; 7] ]
            dict [ 0, [2]; 9, [7] ]
            dict [ 0, [3]; 8, [7] ]
            dict [ 1, [5]; 4, [6] ]
            dict [ 0, [2; 5]; 2, [5]; 4, [6; 7]; 6, [7] ]
            dict [ 0, [3; 6]; 1, [5; 7]; 3, [6]; 5, [7] ]
        |]

    /// 空きマスを取得する。
    let inline getEmpty field = field &&& 15

    /// 指定されたマスのコインの状態を取得する。表向きの場合はSome true、裏向きの場合はSome false、空マスの場合はNone。
    let getCoinState x field = if getEmpty field = x then None else Some(field &&& (1 <<< x + 4) <> 0)

    /// コインが全部裏向きかどうか。
    let inline isAllTail field = field >>> 4 = 0

    /// 指定されたマスのコインを反転する。
    let inline reverse x field = field ^^^ (1 <<< x + 4)

    /// 指定されたマスから空きマスへコインを移動する。移動できない場合は例外を発生する。
    let move x field =
        try
            let reversedCoins = movableAndReversedCoins.[getEmpty field].[x]
            let coin = getCoinState x field |> Option.get |> function true -> 1 | false -> 0
            let inline setCoin field = field ||| (coin <<< getEmpty field + 4)
            let inline setEmpty field = field &&& ~~~15 ||| x &&& ~~~(1 <<< x + 4)
            (field, reversedCoins) ||> List.fold (fun f m -> reverse m f) |> setCoin |> setEmpty
        with
        | :? KeyNotFoundException -> failwith "ロジックエラー"

    /// 指定された盤面から移動可能なコインのうちいずれかを1枚移動させた後の全パターンを取得する。、
    let moveAll field =
        movableAndReversedCoins.[getEmpty field] |> Seq.map (fun (KeyValue(x, _)) -> move x field, x)
        |> Seq.toList

    /// 問題を解く。
    let solve() =
        let memo = HashSet(HashIdentity.Structural)
        /// 問題を解く関数の本体。幅優先探索を行う。
        /// 現在の盤面から次の盤面の全パターンを取得し、既に現れたパターンを除外し、残りのパターンについて再帰的に探索する。
        let rec solve fields =
            match fields |> List.tryFind (fst >> isAllTail) with
            | Some(_, result) -> List.rev result
            | None ->
                fields |> List.collect (fun (field, record) ->
                    field |> moveAll |> List.map (fun (f, r) -> f, r::record))
                |> List.filter (fun (field, record) -> memo.Add field) |> solve

        let initialField = 0b11111111100000
        memo.Add initialField |> ignore
        solve [initialField, []]

    solve()
