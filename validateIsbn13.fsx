/// モジュラス10ウェイト3,1
let modulous10weight31 xs =
    Seq.initInfinite (fun _ -> [1; 3]) |> Seq.concat |> Seq.map2 (*) xs
    |> Seq.sum |> fun s -> (10 - (s % 10)) % 10

/// ISBN13のヘッダとして妥当かどうか
let validateIsbn13Header (isbn : string) = List.exists ((=) isbn.[.. 2]) ["978"; "979"]

/// ISBN13として妥当かどうか
let validateIsbn13 isbn =
    String.length isbn = 13
    && validateIsbn13Header isbn
    && isbn.[.. 11] |> Seq.map (string >> int) |> modulous10weight31 |> (=) (int isbn.[12 .. 12])

/// 数字がひとつ不明なISBN13を復元する
let completeIsbn13 isbn =
    match String.length isbn with 13 -> Some isbn | _ -> None
    |> Option.bind (Seq.mapi (fun i x -> i, x) >> Seq.filter (snd >> (=) '?') >> Seq.toList
                    >> function [i, _] -> Some i | _ -> None)
    |> Option.bind (fun brokenIndex ->
        let checkDigit = int isbn.[12 .. 12]
        let brokenCheckDigit =
            isbn |> Seq.map ((function '?' -> '0' | x -> x) >> string >> int) |> Seq.take 12
            |> modulous10weight31
        let brokenNumber =
            [| [0 .. 9]; [for i in 0 .. 3 .. 27 -> i % 10] |].[brokenIndex % 2]
            |> List.findIndex ((=) ((brokenCheckDigit - checkDigit + 10) % 10))
        let completedIsbn = isbn |> String.map (function '?' -> brokenNumber |> string |> char | x -> x)
        completedIsbn |> validateIsbn13
        |> function
            | true when validateIsbn13Header completedIsbn -> Some completedIsbn
            | _ -> None)

// テスト
validateIsbn13 "9784101092058" |> (=) true |> printfn "%A"
validateIsbn13 "9784062772211" |> (=) true |> printfn "%A"
validateIsbn13 "9784150315684" |> (=) false |> printfn "%A"
completeIsbn13 "9784062?72211" |> (=) (Some "9784062772211") |> printfn "%A"
completeIsbn13 "978415031?684" |> (=) (Some "9784150310684") |> printfn "%A"
