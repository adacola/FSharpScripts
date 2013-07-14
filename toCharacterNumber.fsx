open System
open System.Text

/// 前後の単語に与える影響
type Effect = WeakSokuonbin | StrongSokuonbin | Vanishing | Dakuten of string | Handakuten of string
with
    member this.IsSame = function
    | other when this = other -> true
    | Dakuten _ when (match this with Dakuten _ -> true | _ -> false) -> true
    | Handakuten _ when (match this with Handakuten _ -> true | _ -> false) -> true
    | _ -> false

/// 数字を表す単語
type DigitWord = {
    /// 通常の単語
    DefaultText : string
    /// 前後の単語に受ける影響のリスト
    Affecteds : Effect list
    /// 前の単語に与える影響のリスト
    EffectPrevs : Effect list
    /// 次の単語に与える影響のリスト
    EffectNexts : Effect list
}

let toCharacterNumber number =
    if number < 0I || 99999999999999999999I < number then ArgumentOutOfRangeException("number") |> raise
    /// 数値を下の位からn桁ごとに分割する
    let split n : bigint -> _ =
        string >> Seq.map string >> Seq.toList >> List.rev >> Seq.unfold (function
        | [] -> None
        | xs ->
            let result = xs |> Seq.truncate n |> String.concat ""
            let rest = xs |> Seq.skip result.Length |> Seq.toList
            Some(result, rest))
    /// 数字をひらがな表現に変換する
    let toHiragana number =
        let dw = { DefaultText = ""; Affecteds = []; EffectPrevs = []; EffectNexts= [] }
        let digitWords = [|
            None
            Some { dw with DefaultText = "いち"; Affecteds = [WeakSokuonbin; StrongSokuonbin; Vanishing] }
            Some { dw with DefaultText = "に" }
            Some { dw with DefaultText = "さん"; EffectNexts = [Dakuten ""] }
            Some { dw with DefaultText = "よん" }
            Some { dw with DefaultText = "ご" }
            Some { dw with DefaultText = "ろく"; Affecteds = [StrongSokuonbin]; EffectNexts = [Handakuten ""] }
            Some { dw with DefaultText = "なな" }
            Some { dw with DefaultText = "はち"; Affecteds = [WeakSokuonbin; StrongSokuonbin]; EffectNexts = [Handakuten ""] }
            Some { dw with DefaultText = "きゅう" }
        |]
        let digitOf4Words = [|
            None
            Some { dw with DefaultText = "じゅう"; Affecteds = [WeakSokuonbin; StrongSokuonbin]; EffectPrevs = [Vanishing] }
            Some { dw with DefaultText = "ひゃく"; Affecteds = [Dakuten "び"; Handakuten "ぴ"; StrongSokuonbin];
                           EffectPrevs = [Vanishing; StrongSokuonbin] }
            Some { dw with DefaultText = "せん"; Affecteds = [Dakuten "ぜ"]; EffectPrevs = [Vanishing; WeakSokuonbin] }
        |]
        let digitPer4Words = [|
            None
            Some { dw with DefaultText = "まん" }
            Some { dw with DefaultText = "おく" }
            Some { dw with DefaultText = "ちょう"; EffectPrevs = [WeakSokuonbin] }
            Some { dw with DefaultText = "けい"; EffectPrevs = [StrongSokuonbin] }
        |]
        /// 影響に対応した変換関数を返す
        let transformEffect = function
        | WeakSokuonbin | StrongSokuonbin -> function "" -> "" | str -> str.[.. str.Length - 2] + "っ"
        | Vanishing -> fun _ -> ""
        | Dakuten s | Handakuten s -> function "" -> "" | str -> s + str.[1 ..]
        /// 前後の単語の影響を受けて変化した単語の文字列を返す
        let transform prev this next =
            let this = Option.get this
            let effects =
                (match prev with None -> [] | Some p -> p.EffectNexts) @ (match next with None -> [] | Some n -> n.EffectPrevs)
            this.Affecteds |> List.filter (fun x -> List.exists x.IsSame effects)
            |> List.map transformEffect |> List.fold (|>) this.DefaultText
        if number = 0I then "ぜろ" else
        let digits =
            number |> split 4 |> Seq.mapi (fun i digitOf4s ->
                let ds =
                    digitOf4s |> Seq.mapi (fun j ->
                        string >> int >> Array.get digitWords >> function None -> [] | d -> [digitOf4Words.[j]; d])
                    |> Seq.concat |> Seq.filter Option.isSome |> Seq.toList
                match ds with [] -> [] | _ -> digitPer4Words.[i]::ds)
            |> Seq.concat |> Seq.filter Option.isSome |> Seq.toList |> List.rev
        None::digits @ [None] |> Seq.windowed 3 |> Seq.map (fun [|prev; this; next|] -> transform prev this next)
        |> String.concat ""
    toHiragana number