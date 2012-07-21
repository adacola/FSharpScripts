(*
迷路の最短経路を与えるプログラムを考えよ.
壁は#, 通路は.で表されている.
また迷路の外側は全て壁（即ち#）となっている.
スタート地点はS, ゴール地点はGである.
スタートからゴールまでの道のりを表わせ.
上へ移動する場合にはu, 同様に下はd, 右はr, 左はlとせよ.
最短経路が複数ある場合は, それらのうちどれかひとつを出力せよ。
*)

/// 迷路の最短経路をA*で解く
let solve maze =
    let maze = maze |> Seq.map Seq.toArray |> Seq.toArray
    let pos x =
        maze |> Seq.mapi (fun y m -> y, m)
        |> Seq.tryPick ((<||) (fun y -> Seq.tryFindIndex ((=) x) >> Option.map (fun x -> y, x)))
    let distance (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)
    let rec solve goal cls = function
        | [] -> None
        | ((p, ds), _)::ops when p = goal -> ds |> List.rev |> String.concat "" |> Some
        | (((y, x), ds), (g, h)) as op::ops ->
            let cls = cls |> Map.add (y, x) op
            [(y - 1, x), "u"; (y + 1, x), "d"; (y, x - 1), "l"; (y, x + 1), "r"]
            |> List.choose (fun ((y, x) as p, d) ->
                if maze.[y].[x] <> '#' then Some((p, d::ds), (g + 1, distance goal p)) else None)
            |> List.fold (fun (cls, ops) (((p, _), (g, h)) as x) ->
                match Map.tryFind p cls with
                | Some(_, (cg, ch)) when g + h < cg + ch -> Map.remove p cls, x::ops
                | Some _ -> cls, ops
                | None -> cls, x::ops) (cls, ops)
            |> (fun (cls, ops) -> cls, ops |> Seq.sortBy (snd >> (<||) (+)) |> Seq.distinctBy (fst >> fst) |> Seq.toList)
            ||> solve goal
    match pos 'S', pos 'G' with
    | None, _ | _, None -> invalidArg "maze" "Either start or goal is not found."
    | Some start, Some goal -> solve goal Map.empty [(start, []), (0, distance start goal)]

Seq.initInfinite (fun _ -> System.Console.ReadLine()) |> Seq.takeWhile ((<>) "")
|> solve |> printfn "%A"
