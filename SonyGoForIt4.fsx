open System
open System.Text.RegularExpressions

/// 楽譜をパーズする。
let parse (melody : string) =
    let parsePitch pitch = match Int32.TryParse pitch with true, p -> Some p | false, _ -> None
    let parseLength length =
        let m = Regex.Match(length, @"\A(?<num>\d+)(?<dot>\.?)\Z")
        match m.Success, m.Groups.["num"].Value, m.Groups.["dot"].Value with
        | true, num, "." -> 96 / int num
        | true, num, "" -> 64 / int num
        | _ -> failwith "譜面データの構文エラー"
    let parseAttribute = function
    | "" -> 0 | "s" -> 1 | "x" -> 2 | "b" -> -1 | "d" -> 2
    | _ -> failwith "譜面データの構文エラー"
    let toHalfTone pitch attribute =
        pitch |> Option.map (fun p ->
            let octave = (if p < 0 then p - 6 else p) / 7
            let halfTone =
                match (p % 7 + 7) % 7 with
                | 1 -> 1 | 2 -> 3 | 3 -> 5 | 4 -> 6 | 5 -> 8 | 6 -> 10 | _ -> 0
            octave * 12 + halfTone + attribute)
    melody.Split([|','|]) |> Array.map (fun note ->
        let n = note.Split([|':'|])
        let pitch, length, attribute = parsePitch n.[0], parseLength n.[1], parseAttribute n.[2]
        toHalfTone pitch attribute, length)

/// パーズされた楽譜の特徴量を求める。
/// 評価方法は、「(隣り合う2音符の半音単位での音程の差) + (隣り合う2音符の長さの和)」の合計
let evaluate notes =
    notes |> Seq.choose (fun (pitch, length) -> pitch |> Option.map (fun p -> p, length))
    |> Seq.pairwise
    |> Seq.sumBy (fun ((p1, l1), (p2, l2)) -> (p1 - p2) + (l1 + l2))

/// 与えられた特徴量を持つ楽譜を生成する。
/// 生成された楽譜は、最初と最後の音がG4かつ旋律長が全音符3個～4個の範囲内という条件を満たす。
let makeMelody evaluation =
    let basePitch = Some -2
    let normalize pitch length =
        ((length, []), [64; 32; 16; 8; 4; 2]) ||> List.fold (fun (len, result) note ->
            len % note, result @ List.replicate (len / note) (pitch, note))
        |> snd
    let format melody =
        let convert = function 3 -> "32." | length -> 64 / length |> string
        melody |> Seq.map (function
        | None, length -> sprintf ":%s:" <| convert length
        | Some pitch, length -> sprintf "%d:%s:" <| pitch <| convert length)
        |> String.concat ","
    if evaluation = 0 then
        List.replicate 4 (None, 64)
    else
        let first = if evaluation % 2 = 1 then 3 else 2
        let last = if (evaluation - first) % 4 = 0 then 4 else 2
        let inner = (evaluation - first - last) / 2
        let rest = 64 * 4 - (first + inner + last)
        (basePitch, first)::normalize basePitch inner @ ((basePitch, last)::normalize None rest)
    |> format

let melodies =
    Map.ofList [
        "A", "-3:4:,-2:4:,-1:4:,0:4:b,-1:4:,-2:4:,-3:4:,:4:,-1:4:,0:4:b,1:4:,2:4:,1:4:,0:4:b,-1:4:,:4:"
        "B", "3:4:,2:4:,1:4:,0:4:b,1:4:,2:4:,3:4:,:4:,1:4:,0:4:b,-1:4:,-2:4:,-1:4:,0:4:b,1:4:,:4:"
        "C", "3:4:,2:4:,1:4:,0:4:,1:4:,2:4:,3:4:,:4:,1:8:,0:4.:,-1:4:,-2:4:,:8:,-1:4.:,0:8:,1:8:"
        "D", "-6:8:,-6:8:,-6:8:,-4:8:,-2:8:,-2:8:,-2:8:,:8:,-5:8:,-5:8:,-5:8:,-3:8:,-2:8:,-2:8:,-2:8:,:8:,:8:,-2:8:,-2:8:,-1:8:b,-1:8:,-1:8:,-1:8:,-1:8:b,-2:4:,0:4:,1:4:,:4:"
        "E", "-6:8.:,-7:16:,-6:8:,-5:8:,-4:8:,-4:8:,-4:4:,-3:8.:,-4:16:,-5:8:,-6:8:,-5:4:,:4:,-3:8:,-5:4:,-5:8:,-4:8:,-4:8:,-3:8:,-3:8:,-4:8:,-4:8:,-5:8:,-5:8:,-6:4:,:4:"
        "F", "-6:2:,-5:4:,-6:8:,-5:8:,-4:4:,-2:4:,-4:4:,:4:,-5:4:,-5:4:,-6:4:,-5:4:,-4:2.:,:4:"
        "G", "-2:8.:,-1:16:,-2:8.:,-1:16:,-2:4:,-4:4:,-4:8.:,-3:16:,-4:8.:,-3:16:,-4:4:,-6:4:,-4:8:,:8:,-6:8.:,-5:16:,-4:8:,:8:,-6:8.:,-5:16:,-4:8.:,-4:16:,-2:8.:,-2:16:,-5:8.:,-4:16:,-5:4:"
    ] |> Map.map (fun _ -> parse >> evaluate)
let melody = makeMelody melodies.["G"]
let parsedMelody = melody |> parse
let melodyEvaluation = parsedMelody |> evaluate
let melodyLength = parsedMelody |> Seq.sumBy snd
