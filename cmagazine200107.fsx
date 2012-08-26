(*
　Ｆｉｇ．２のような盤を用意し，Ａ軍の戦車３台とＢ軍の戦車３
台をそれぞれ１，２，３と１６，１７，１８の円に配置する。直線
は道路を，円（数字のマス）は広場を表す。

　１．Ａ軍を先手として，３台の戦車のうちのどれかを１台動かす。
　　　これを交互に繰り返す

　２．戦車は道路に沿って広場から広場へと移動できる。１つの広
　　　場には１台しか入れない

　３．戦車は動き始めたら止まるまで直進しかできない。いくつ先
　　　の広場で止まるかは自由だが，通り道にほかの戦車がいる場
　　　合はそれより先には進めない。いくつ進んでもこれが「１手」

　４．止まった広場が敵軍の戦車から道路を介してまっすぐに見え
　　　る場合には，撃たれてしまう

　以上の条件で，両軍が撃ち合うことなく戦車をそっくり入れ替え
たい（Ａ軍とＢ軍がそれぞれ１６，１７，１８と１，２，３にある
状態にしたい）。最低何手で完成できるだろうか。その移動手順の
一例も示すこと。

Ｆｉｇ．２　タンク・チェンジの盤面

　　１　　　６　　　11　　　16
　　　＼　／　＼　／　＼　／
　　　　４　　　９　　　14
　　　／　＼　／　＼　／　＼
　　２　　　７　　　12　　　17
　　　＼　／　＼　／　＼　／
　　　　５　　　10　　　15
　　　／　＼　／　＼　／　＼
　　３　　　８　　　13　　　18
*)
open System.Collections.Generic

/// 問題を解く。
/// (問題を解く最低手数, 移動手順のリスト)を返す。移動手順のリストの各要素は(移動する戦車の位置, 移動後の位置)。
let solve() =
    let inline swap (a, b) = b, a

    /// すべての道路。
    let allLoads =
        [
            [1 .. 3 .. 13]
            [2 .. 2 .. 6]
            [2 .. 3 .. 8]
            [3 .. 2 .. 11]
            [6 .. 3 .. 18]
            [8 .. 2 .. 16]
            [11 .. 3 .. 17]
            [13 .. 2 .. 17]
        ]

    /// 2つの戦車が同一道路上にあるかどうか。
    let isOnSameLoad tank1 tank2 =
        allLoads |> List.exists (fun load -> [tank1; tank2] |> List.forall (fun t -> List.exists ((=) t) load))

    /// 指定された戦車が位置する広場があるすべての道路を取得する。
    let getLoads tank = allLoads |> List.filter (List.exists ((=) tank))

    /// 指定された戦車が移動できるすべての広場を取得する。
    /// 戦車は、味方戦車に阻まれるまで道路を一直線に移動できる。
    /// また、移動先の広場が属するすべての道路上に敵戦車がいてはならない。
    let getMovableSpaces tank friends enemies =
        tank |> getLoads |> List.collect
            (List.partition ((>) tank) >> (fun (less, greater) -> [less |> List.rev; greater |> List.filter ((<>) tank)])
             >> List.collect (Seq.takeWhile (fun space -> friends |> Set.contains space |> not) >> Seq.toList))
        |> List.filter (fun space -> enemies |> Set.forall (isOnSameLoad space >> not))

    /// 味方戦車が移動できるすべてのパターンを取得する。
    let getAllMovingPattern friends enemies =
        friends |> Seq.collect (fun tank ->
            getMovableSpaces tank friends enemies
            |> Seq.map (fun space -> (tank, space), (friends |> Set.remove tank |> Set.add space, enemies)))
        |> Seq.toList

    /// 戦車の初期配置。(A軍戦車の位置, B軍戦車の位置)
    let initialPattern = set [1; 2; 3], set [16; 17; 18]

    /// 戦車の最終配置。(A軍戦車の位置, B軍戦車の位置)
    let finalPattern = swap initialPattern

    /// 問題を解く。
    let solve() =
        let memo = HashSet(HashIdentity.Structural)
        let rec solve isA patterns =
            let getAbsolutePattern tanks = if isA then tanks else swap tanks
            match patterns |> List.tryFind (fun (_, tanks) -> getAbsolutePattern tanks = finalPattern) with
            | Some(record, _) -> List.length record, List.rev record
            | None ->
                patterns |> List.collect (fun (record, tanks) ->
                    tanks ||> getAllMovingPattern |> List.choose (fun (moving, tanks) ->
                        if tanks |> getAbsolutePattern |> memo.Add then Some(moving::record, swap tanks) else None))
                |> solve (not isA)
        memo.Add initialPattern |> ignore
        solve true [[], initialPattern]

    solve()
