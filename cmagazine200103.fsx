(*
　Fig. 1のような六角形のマスの真ん中にコインを置く。ただ
し，「どんな直線も，3枚以上のコインの中心を通ってはいけな
い」という条件付きだ。最大何枚のコインを置けるだろうか。
また，その最大枚数での配置には，全体を回転した解・鏡像の
解を除いて何通りあるだろうか。Fig. 2では10枚のコインが置
いてあるが，太線で示すところがマズいことになる。

 Fig.１六角形のマス　　Fig.２直線上に並ぶ例
　 ○○○○　　　　　　　 ○●○○ ／
　○○○○○　　　　　　 ●○○○●
 ○○○○○○　　　　　 ○○○●○●
○○○○○○○　　　　 ○○○○○○○
 ○○○○○○　　　　　 ●○○○○●
　○○○○○　　　　　／ ○○●●○
　 ○○○○　　　　　　　 ●○○○
*)

open System.Collections.Generic

let solve() =
    let inline cons x xs = x::xs

    /// フィールドの行ごとのマス数。
    let rowCounts = [|for i in -3 .. 3 -> 7 - abs i|]

    /// フィールドの行ごとのx座標開始番号
    let rowStarts = [|0; 0; 0; 0; 1; 2; 3|]

    /// フィールドの全マス数
    let nums = rowCounts |> Seq.sum

    /// コインを置くマスの座標と、対応するマス番号の辞書。
    let posToNum =
        Seq.zip rowStarts rowCounts |> Seq.mapi (fun y (x0, c) -> Seq.init c (fun dx -> y, x0 + dx))
        |> Seq.concat |> Seq.mapi (fun i p -> p, i) |> Map.ofSeq

    /// マス番号と、対応するマスの座標の辞書。
    let numToPos = posToNum |> Map.toArray |> Array.map fst

    /// 「ある2マスに対し、2マスの中心を通る直線上にある他のマスのリスト」の辞書。
    let othersOnLine =
        (Map.empty, [for i in 0 .. nums - 2 do for j in i + 1 .. nums - 1 -> i, j])
        ||> List.fold (fun m -> function
            | ijs when Map.containsKey ijs m -> m
            | i, j ->
                let (iy, ix), (jy, jx) = numToPos.[i], numToPos.[j]
                let dy, dx = jy - iy, jx - ix
                let line =
                    (iy, ix) |> Seq.unfold (fun (y, x) ->
                        posToNum |> Map.tryPick (fun (y', x') n -> if y' = y + dy && x' = x + dx then Some(n, (y', x')) else None))
                    |> Seq.toList |> cons i
                (m, [for p1 in line do for p2 in line do if p1 <> p2 then yield p1, p2])
                ||> List.fold (fun m (p1, p2) -> m |> Map.add (p1, p2) (line |> List.filter (fun l -> l <> p1 && l <> p2))))

    /// 元の盤面と反転させた盤面をそれぞれ60°ずつ回転させた全盤面を取得する。
    let getReverseAndRotates =
        let reverse =
            (0, rowCounts) ||> Seq.scan (+) |> Seq.skip 1 |> Seq.zip rowCounts
            |> Seq.collect (fun (c, s) -> Seq.init c ((-) (s - 1))) |> Seq.toArray |> Array.get
        let rotate =
            [|for i in 0 .. nums - 1 -> let y, x = numToPos.[i] in posToNum.[x, rowCounts.[x] - 1 - y + rowStarts.[x] * 2]|]
            |> Array.get
        let allRotates = Seq.unfold (fun xs -> Some(xs, xs |> Set.map rotate)) >> Seq.take 6
        fun xs -> [xs; xs |> Set.map reverse] |> Seq.collect allRotates

    /// 問題を解く。
    let solve() =
        let result = HashSet(HashIdentity.Structural)
        let rec solve maxCount coins emptyPoss =
            let count = Set.count coins + Set.count emptyPoss
            match emptyPoss |> Set.isEmpty, compare maxCount count |> sign with
            // 現在のコイン数と残りマス数を足しても最大コイン数に満たない場合は探索を打ち切る。
            | _, 1 -> maxCount
            // 現在のパターンについて探索完了し、現在のコイン数が最大コイン数を上回った場合、現在のコイン数を新たな最大コイン数とする。
            // また、今までのコイン配置パターンをクリアし、現在のコイン配置を新たなコイン配置パターンに設定する。
            | true, -1 -> result.Clear(); result.Add coins |> ignore; count
            // 現在のパターンについて探索完了し、現在のコイン数が最大コイン数と同じ場合、
            // 回転・反転盤面が既にコイン配置パターンに登録されているかチェックする。
            // 回転・反転盤面がコイン配置パターンに登録されていない場合のみ、現在のコイン配置をコイン配置パターンに追加する。
            | true, 0 when coins |> getReverseAndRotates |> Seq.exists result.Contains -> maxCount
            | true, 0 -> result.Add coins |> ignore; maxCount
            // 現在のパターンについてまだ探索が終わっていない場合
            | _ ->
                // 残りの空きマスのうち最も若い番号を対象とし、そこにコインを置いた場合と置かなかった場合の2通りについて再帰的に探索し、
                // 最終的にコインをより多く置けた方を選択する。
                // コインを置いた場合は、既に置かれているコインとの直線上にあるマスを残りの空きマスから除外する。
                let pos = Set.minElement emptyPoss
                let emptyPoss = emptyPoss |> Set.remove pos
                let setPoss = coins |> Seq.collect (fun c -> othersOnLine.[c, pos]) |> Set.ofSeq |> (-) emptyPoss
                let setCount = solve maxCount (Set.add pos coins) setPoss
                solve setCount coins emptyPoss
        let maxCount = set [0 .. nums - 1] |> solve 0 Set.empty
        maxCount, result.Count

    solve()
