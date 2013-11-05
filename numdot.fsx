let start = [|[1; 2; 3; 4; 5]|]
let goal = [|[1]; [2]; [3]; [4]; [5]|]
let maxTarget = 5

let solve() =
    let tail =
        (start, maxTarget) |> Seq.unfold (fun (nums, target) ->
            if nums = goal then None else
            let uniqTarget = [target]
            if nums |> Array.exists ((=) uniqTarget) then
                let nums' = nums |> Array.filter ((<>) uniqTarget)
                nums'.[0] <- target::nums'.[0] |> List.sort
                Some(None, (nums', target - 1))
            else
                let i = nums |> Array.findIndex (List.exists ((=) target))
                let nums' =
                    Array.init (nums.Length + if i = nums.Length - 1 then 1 else 0) (fun j ->
                        if j = i then List.filter ((<>) target) nums.[j]
                        elif j = nums.Length then []
                        else nums.[j])
                nums'.[i + 1] <- target::nums'.[i + 1] |> List.sort
                Some(Some nums', (nums', maxTarget)))
        |> Seq.choose id |> Seq.toList
    start::tail

let test expected actual =
    let resultToStrings (result : int list[] list) = result |> List.map (Seq.map (Seq.map string >> String.concat "") >> String.concat ".")
    let actual = actual |> resultToStrings
    if List.length expected <> List.length actual then Choice2Of2 "expected と actual の要素数が一致しません" else
    (Choice1Of2 [], expected, actual) |||> List.fold2 (fun result e a ->
        match result with
        | Choice1Of2 r ->
            match e with
            | None -> Choice1Of2(a::r)
            | Some e when e = a -> Choice1Of2(a::r)
            | Some e -> sprintf "Expected  : %A , Actual : %A" e a |> Choice2Of2
        | Choice2Of2 _ -> result)
    |> function Choice1Of2 r -> Choice1Of2(List.rev r) | otherwise -> otherwise

let expected = [
        Some "12345"
        Some "1234.5"
        Some "1235.4"
        Some "123.45"
        Some "123.4.5"
        Some "1245.3"
        Some "124.35"
        Some "124.3.5"
        Some "125.34"
        Some "12.345"
        Some "12.34.5"
        Some "125.3.4"
        Some "12.35.4"
        Some "12.3.45"
        Some "12.3.4.5"
        Some "1345.2"
        Some "134.25"
        Some "134.2.5"
        Some "135.24"
        Some "13.245"
        Some "13.24.5"
        Some "135.2.4"
        Some "13.25.4"
        Some "13.2.45"
        Some "13.2.4.5"
        Some "145.23"
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        None
        Some "1.2.34.5"
        Some "15.2.3.4"
        Some "1.25.3.4"
        Some "1.2.35.4"
        Some "1.2.3.45"
        Some "1.2.3.4.5"
    ]
    
let main() =
    match solve() |> test expected with
    | Choice1Of2 result ->
        result |> List.iter (printfn "%s")
        printfn "ENV:F#"
        printfn "POINT:"
    | Choice2Of2 msg -> failwith msg
