let toOrdinalNumber n =
    if n < 1 then invalidArg "n" "1以上の整数を指定してください"
    let postfix n =
        match n % 100, n % 10 with
        | 11, _ | 12, _ | 13, _ -> "th"
        | _, 1 -> "st"
        | _, 2 -> "nd"
        | _, 3 -> "rd"
        | _ -> "th"
    n |> postfix |> sprintf "%d%s" n
