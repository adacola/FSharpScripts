open System

/// 指定された年（西暦）の日数を取得する。
let daysOfYear = DateTime.IsLeapYear >> function true -> 366 | _ -> 365

/// 明治以降の元号
type Era = Meiji | Taisho | Showa | Heisei with
    member __.MinYear = 1
    member x.MaxYear = match x with Meiji -> 45 | Taisho -> 15 | Showa -> 64 | Heisei -> DateTime.Today.Year - 1988
    member x.Next = match x with Meiji -> Some Taisho | Taisho -> Some Showa | Showa -> Some Heisei | Heisei -> None
    static member All = [Meiji; Taisho; Showa; Heisei]

/// 指定された年（明治以降の和暦）の日数を取得する。
/// 注意点1：年の途中で元号が切り替わっているのに対応する必要あり。
/// 注意点2：明治5年までは太陰太陽暦、明治6年からグレゴリオ暦なのを考慮する必要あり。
/// 注意点3：今年中に新元号に変わる可能性が0ではないため、今年の日数は未確定である。
let daysOfYearInJapaneseCalendar =
    let specialFirstDateTimes =
        dict [(Meiji, 1), DateTime(1868, 1, 25)
              (Meiji, 2), DateTime(1869, 2, 11)
              (Meiji, 3), DateTime(1870, 2, 1)
              (Meiji, 4), DateTime(1871, 2, 19)
              (Meiji, 5), DateTime(1872, 2, 9)
              (Taisho, 1), DateTime(1912, 7, 30)
              (Showa, 1), DateTime(1926, 12, 25)
              (Heisei, 1), DateTime(1989, 1, 8)]
    let getGregorianFirstDateTime era year =
        match specialFirstDateTimes.TryGetValue((era, year)) with
        | true, d -> d
        | false, _ ->
            DateTime((match era with Meiji -> 1867 | Taisho -> 1911 | Showa -> 1925 | Heisei -> 1988) + year, 1, 1)
    let getNextYear (era : Era) year =
        if year < 1 || era.MaxYear < year then invalidArg "year" "暦の範囲外です"
        if year = era.MaxYear then era.Next |> Option.map (fun e -> e, e.MinYear) else Some(era, year + 1)
    fun era year ->
        getNextYear era year |> Option.map (fun next ->
            let [d; nd] = [era, year; next] |> List.map ((<||) getGregorianFirstDateTime)
            (nd - d).Days)

printfn "1900 - 2012"
[for year in 1900 .. 2012 -> year, daysOfYear year] |> List.iter (printfn "%A")
printfn "Meiji 1 - Heisei 24"
[for era in Era.All do
 for year in era.MinYear .. (if era = Heisei then 24 else era.MaxYear) ->
    era, year, daysOfYearInJapaneseCalendar era year]
|> List.iter (printfn "%A")
