open System

/// 問題を解く。
let solve years n =
    let getTime a b c n =
        if n < 0 then invalidArg "n" "非負整数を指定してください。"
        let birthday = DateTime(a, b, c)
        let today = DateTime.Today
        // 法律では、満年齢が上がるのは誕生日の前日が終了したタイミングとなるため、
        // 没日を誕生年月日のn+1年後に設定すれば題意を満たす。
        // ただし誕生日が2/29の場合、寿命の年が閏年であれば没日を2/29とし、平年であれば没日を3/1とする。
        let deathDate =
            match b, c with
            | 2, 29 when DateTime.IsLeapYear (a + n + 1) |> not -> DateTime(a + n + 1, 3, 1)
            | _ -> DateTime(a + n + 1, b, c)
        if deathDate <= today then invalidArg "n" "没日が今日の日付の後になるように指定してください。"
        let seconds =
            [today; deathDate] |> List.map (fun d -> (d - birthday).TotalSeconds)
            |> List.reduce (/) |> (*) 86400. |> int
        let h, m, s = seconds / 3600, (seconds % 3600) / 60 , (seconds % 3600) % 60
        let dateToString (d : DateTime) = d.ToString "yyyy/MM/dd"
        printfn "誕生日が%s、寿命が%d年の場合、今日%sは%d時%d分%d秒にあたる"
            (dateToString birthday) n (dateToString today) h m s
    for a in years do
        for b in [1 .. 12] do
            for c in [1 .. DateTime.DaysInMonth(a, b)] do
                getTime a b c n

printfn "i)"
solve [1990 .. 2000] 80
printfn "ii)"
solve [1900 .. 2000] 200
