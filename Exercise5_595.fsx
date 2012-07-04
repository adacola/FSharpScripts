(*
【　課題　】DB上に給与の支給テーブルがあります。
　　　　　　支店・部・課・社員番号・基本給・役職手当・残業時間
　　　　　　これらの情報をDBから読み込んだJYOUHOUDATA.javaから、
　　　　　　各支店ごとの部、課の合計、支店合計
　　　　　　総合計を求めなさい。最後に全ての合計を印字する事。
　　　　　　集計項目は基本給・役職手当・残業手当・総支給額の4項目とする。
【　補足　】残業手当　＝（基本給＋役職手当）/160＊1.2
　　　　　　総支給額　＝　基本給＋役職手当＋残業手当　で計算します。
*)

type 給与の支給 = {
    支店 : string
    部 : string
    課 : string
    社員番号 : int
    基本給 : decimal
    役職手当 : decimal
    残業時間 : int
}

let 残業手当 { 基本給 = k; 役職手当 = y; 残業時間 = z } =
    (k + y) / 160M * 1.2M * decimal z

let 総支給額 d = d.基本給 + d.役職手当 + 残業手当 d

let 合計を印字 給与支給データ =
    let グルーピング方法リスト = [
        "支店, 部", fun d -> [d.支店; d.部]
        "支店, 課", fun d -> [d.支店; d.課]
        "支店", fun d -> [d.支店]
        "すべて", fun _ -> ["総合計"]
    ]
    let 給与計算方法リスト = [
        "基本給", fun d -> d.基本給
        "役職手当", fun d -> d.役職手当
        "残業手当", 残業手当
        "総支給額", 総支給額
    ]
    for gn, gf in グルーピング方法リスト do
        printfn "グルーピング方法 : %s" gn
        給与支給データ |> Seq.groupBy gf |> Seq.iter (fun (g, ds) ->
            g |> String.concat ", " |> printfn "\tグループ : %s"
            for kn, kf in 給与計算方法リスト do
                ds |> Seq.sumBy kf |> printfn "\t\t%sの合計額 : %M" kn)
