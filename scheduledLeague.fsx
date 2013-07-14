(*
試合方法　総当り
出場チーム数　13チーム
一度で同時に行える試合数（コートの面数）　5試合
この条件にて、5面のコートをA、B、C、D、E とした時
全78試合の行われる組み合わせを列挙せよ。
*)

let scheduledLeague matchCount teamCount =
    if teamCount <= 0 then invalidArg "teamCount" "正の数を入力してください"
    if matchCount <= 0 then invalidArg "matchCount" "正の数を入力してください"

    let league teams =
        let rotate teams =
            let len = Array.length teams
            teams |> Array.permute (function
                | i when i = len - 1 -> i
                | i -> (i + 1) % (len - 1))
        let pairwise teams =
            let len = Array.length teams
            [ for i in 0 .. len / 2 - 1 do
                match teams.[i], teams.[len - 1 - i] with
                | Some x, Some y -> yield [x; y]
                | _ -> () ]
        let teams =
            [| for team in teams do yield Some team
               if List.length teams % 2 = 1 then yield None |]
        (Array.length teams - 1, teams) |> Seq.unfold (function
            | 0, _ -> None
            | i, ts -> Some(pairwise ts, (i - 1, rotate ts)))
        |> Seq.toList

    let schedule matchCount allLeagueMatches =
        let partition count teams =
            let now = teams |> Seq.truncate count |> Seq.toList
            let next = teams |> Seq.skip (List.length now) |> Seq.toList
            now, next
        allLeagueMatches |> Seq.unfold (function
            | [] | []::[] -> None
            | ts::rest when List.length ts >= matchCount ->
                let (now, next) = partition matchCount ts
                Some(now, next::rest)
            | ts::[] -> Some(ts, [])
            | ts1::ts2::rest ->
                let (available, unavailable) =
                    ts2 |> List.partition (List.forall (fun t -> ts1 |> List.forall (List.forall ((<>) t))))
                let (now, next) = available |> partition (matchCount - List.length ts1)
                Some(ts1 @ now, (next @ unavailable)::rest))
        |> Seq.toList

    let print allLeagueMatches =
        allLeagueMatches |> List.iteri (fun i ts ->
            printfn "第%d回目" <| i + 1
            ts |> Seq.map (Seq.map string >> String.concat "-")
            |> Seq.zip (Seq.init matchCount ((+) (int 'A') >> char))
            |> Seq.iter ((<||) (printfn "%c %s")))

    [1 .. teamCount] |> league |> schedule matchCount |> print
