// 長針と短針の間の角度がX度のとき、可能性のある時刻をすべて挙げよ。

let degreeOnClock x =
    let xs = let x' = abs x % 360 in [x'; (360 - x') % 360] |> Seq.distinct
    [for i in 0 .. 21 do
     for x in Seq.sort xs ->
        let s = float (i * 360 + x) * 120. / 11. |> int
        sprintf "%02d:%02d:%02d" (s / 3600) (s / 60 % 60) (s % 60)]
