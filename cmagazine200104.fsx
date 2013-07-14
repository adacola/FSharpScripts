(*
Fig. 4のように，3×3のマスの中に9個の数を書き込んでみた。こ
こで，隣り合った2数(ナナメは考えない)の差の絶対値を｢隣差｣と
呼ぶことにする。すると，Fig. 4にある12か所の隣差において，
1～12すべての数が登場している。このように，隣差が1～12にな
る，9個の数の配置は何通りあるか。なお，書き込む9個の数は1以
上の相異なる整数とし，回転・鏡像の解を除く。また，ある解が
あったときに，それを構成する9個の数全体に同じ数を足したもの
も解となってしまうので，必ず“1”は使うものとする。

 Fig.4 隣差方陣の例
┌─┬─┬─┐
│６│１│13│
├─┼─┼─┤
│14│10│20│
├─┼─┼─┤
│８│７│18│
└─┴─┴─┘
*)

let solve() =
    /// 隣差を決定する順番の共通部分。
    let diffs = [1, 2; 0, 3; 4, 5; 3, 6; 6, 7; 7, 8]

    /// 新たな隣差およびマスの値が妥当であれば追加する。
    /// 妥当かどうかの判断条件は、隣差およびマスが初出であること、隣差は1～12の範囲内であること、マスは1以上であること。
    /// この条件のうち、引数で与えられた隣差については初出の値であることが事前条件となっているためチェックしない。
    /// マスを追加する際、引数で与えられた隣差以外にも新たな隣差が決定することがあるので、それについても妥当であれば追加する。
    let tryAdd (p, pk) dv ps ds pvs dves =
        let pv, dv = Map.find p ps + dv, abs dv
        if pv >= 1 && pvs |> Set.contains pv |> not
        then
            let nds =
                let lines (f1, f2) = [for d in [-1; 1] do let p = f1 pk 3 + d in if 0 <= p && p < 3 then yield pk + f2 d]
                [(/), (*) 3; (%), id] |> List.collect lines |> List.choose (fun nk ->
                    Map.tryFind nk ps |> Option.map (fun nv -> (min pk nk, max pk nk), pv - nv |> abs))
            if nds |> Seq.countBy snd |> Seq.forall (fun (d, c) -> c = 1 && dves |> Set.contains d)
            then
                let ds = (ds, nds) ||> List.fold (fun m (k, v) -> Map.add k v m)
                let dves = nds |> Seq.map snd |> Set.ofSeq |> (-) dves
                Some(Map.add pk pv ps, ds, Set.add pv pvs, dves)
            else None
        else None

    /// 条件を満たすすべての盤面。
    let results =        
        [ // 上段左のマスが1の場合。(マスの初期値, 初期の隣差を決定する順番, 回転および鏡像の盤面を排除する条件式)
          Map.ofList [0, 1], [0, 1; 1, 4], (fun ps _ -> Map.find 1 ps < Map.find 3 ps)
          // 上段中央のマスが1の場合。
          Map.ofList [1, 1], [1, 0; 1, 4], (fun ps _ -> Map.find 0 ps < Map.find 2 ps)
          // 中段中央のマスが1の場合。
          Map.ofList [4, 1], [4, 1; 1, 0], (fun ps ds -> (Map.find 1 ps = 2 && Map.find 0 ps < Map.find 2 ps) || Map.find (0, 1) ds = 1)
        ] |> Seq.collect (fun (ps, ds, cond) ->
            let rec solve ps ds pvs dves = function
                | [] -> Seq.singleton (ps, ds)
                | dk::dks ->
                    seq { for ad in dves do for d in [ad; -ad] -> tryAdd dk d ps ds pvs dves }
                    |> Seq.choose id |> Seq.collect (fun (ps, ds, pvs, dves) -> solve ps ds pvs dves dks)
            ds @ diffs |> solve ps Map.empty (set [1]) (set [1 .. 12]) |> Seq.filter ((<||) cond))

    results |> Seq.length
