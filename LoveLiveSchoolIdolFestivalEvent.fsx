/// 目標ポイント(イベントの目玉景品がもらえるptなどを設定)
let targetPoint = 8000
///　イベント曲をプレイするごとにもらえるpt。
/// 今までのところ、Hardだと最高233pt、1ミスで227ptなので、そのあたりに設定します。
let gotPointByEvent = 200
/// イベント曲を1回プレイするために必要なマーク(マカロンなど)。Hard前提。
let requiredMark = 45
/// 1曲プレイするのにかかる平均時間(分)
let minutesForSong = 2.5
/// LPが1回復するまでにかかる時間(分)
let minutesForLP = 6.

type Song = {
    /// 曲をプレイするのに必要なLP
    RequiredLP : int
    /// 取りこぼしなしの場合に獲得できるマーク(マカロンなど)の数
    MarkCount : int
}

/// 一般曲の難易度Hardの曲のデータ
let hardSong = { RequiredLP = 15; MarkCount = 16 }
/// 特別曲目の難易度Hardの曲のデータ
let specialSong = { RequiredLP = 25; MarkCount = 27 }

type Result = {
    /// イベント曲以外の曲をプレイする必要がある回数
    Count : int
    /// イベント曲以外の曲を必要な回数プレイした場合に蓄積されているマーク(マカロンなど)の数
    Marks : int
    /// イベント曲以外の曲を必要な回数プレイした場合に蓄積されているpt数
    Points : int
    /// イベント曲以外の曲を必要な回数プレイした場合にかかる時間(分)。LP回復の待ち時間も含めます。
    Minutes : float
    /// イベント曲以外の曲を必要な回数プレイした場合に消費するラブカストーンの数。休憩なしの前提。
    Stones : int
    /// イベント曲のプレイ回数も含めた、プレイする必要がある回数
    TotalCount : int
    /// イベント曲のプレイ時間も含めた、目標pt到達までにかかる時間(分)
    TotalMinutes : float
}

/// イベントにて目標ptに到達するまでのデータを計算します。
/// maxLP : 最大LP。
/// mark : 現在既に獲得しているマーク(マカロンなど)の数。
/// point : 現在既に獲得しているpt数。
/// songs : プレイする曲目のリスト。リスト順に曲をプレイした後、ラブカストーンを消費してLP回復する前提。リストの途中でLP不足になる場合は、次の曲のプレイに必要な分のLPの自然回復時間も計算に含めます。
let calcEvent maxLP mark point songs =
    let songs = songs |> Seq.sortBy (fun { MarkCount = m } -> -m) |> Seq.toArray
    let requiredLP = songs |> Array.sumBy (fun { RequiredLP = r } -> r)
    let timePer1Song = (float songs.Length * minutesForSong + float (max 0 (requiredLP - maxLP)) * minutesForLP) / float songs.Length
    let count, marks, points =
        (0, mark, point) |> Seq.unfold (fun (i, mark, point) ->
            let song = songs.[i % songs.Length]
            let r = i + 1, mark + song.MarkCount, point + song.MarkCount
            Some(r, r))
        |> Seq.find (fun (i, mark, point) -> mark / requiredMark * gotPointByEvent + point >= targetPoint)
    let minutes = timePer1Song * float count
    let eventCount = marks / requiredMark
    let eventMinutes = minutesForSong * float eventCount
    {   Count = count
        Marks = marks
        Points = points
        Minutes = minutes
        Stones =  count / songs.Length
        TotalCount = count + eventCount
        TotalMinutes = minutes + eventMinutes
    }
