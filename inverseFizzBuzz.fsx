let inverseFizzBuzz input =
    let lcmWord, lcmNumber = "fizzbuzz", 15
    let partialResultMap =
        [0 .. lcmNumber] |> List.choose (fun n ->
            match n % 3, n % 5 with
            | 0, 0 -> Some("fizzbuzz", n)
            | 0, _ -> Some("fizz", n)
            | _, 0 -> Some("buzz", n)
            | _ -> None)
        |> fun ls -> [1 .. ls.Length] |> Seq.collect (fun n -> Seq.windowed n ls)
        |> Seq.map (Array.unzip >> fun (f, s) -> List.ofArray f, [|s.[s.Length - 1]; s.[0]|])
        |> Seq.sortBy (snd >> Array.reduce (-))
        |> Seq.distinctBy fst
        |> Map.ofSeq
        |> Map.add [lcmWord] [|lcmNumber; lcmNumber|]

    let getPartialResult start words =
        Map.tryFind words partialResultMap |> Option.map (Array.map ((+) start))

    (Array.ofSeq input, []) |> Seq.unfold (function
    | [||], _ -> None
    | words, first ->
        match Array.tryFindIndex ((=) lcmWord) words with
        | Some i -> Some(first @ List.ofArray words.[.. i], (words.[i + 1 ..], [lcmWord]))
        | None -> Some(first @ List.ofArray words, ([||], [lcmWord])))
    |> Seq.toArray
    |> Array.mapi ((*) lcmNumber >> getPartialResult)
    |> function
    | xs when Array.forall Option.isSome xs -> Some(xs.[0].Value.[1], xs.[xs.Length - 1].Value.[0])
    | _ -> None

[
    ["fizz"]
    ["buzz"]
    ["fizzbuzz"]
    ["fizz"; "buzz"]
    ["buzz"; "fizz"]
    ["fizz"; "buzz"; "fizz"]
    ["fizz"; "fizz"]
    ["fizz"; "fizz"; "buzz"]
    ["buzz"; "buzz"]
    ["fizz"; "fizzbuzz"]
    ["fizzbuzz"; "fizz"]
    ["fizz"; "fizzbuzz"; "fizz"]
    ["fizzbuzz"; "fizz"; "buzz"; "fizz"; "fizz"; "buzz"; "fizz"; "fizzbuzz"]
    ["fizz"; "fizzbuzz"; "fizz"; "buzz"; "fizz"; "fizz"; "buzz"; "fizz"; "fizzbuzz"; "fizz"; "buzz"; "fizz"; "fizz"; "buzz"; "fizz"; "fizzbuzz"; "fizz"; "buzz"]
    [""]
    ["abc"]
] |> List.map inverseFizzBuzz |> List.zip [
    Some(3, 3)
    Some(5, 5)
    Some(15, 15)
    Some(9, 10)
    Some(5, 6)
    Some(3, 6)
    Some(6, 9)
    Some(6, 10)
    None
    Some(12, 15)
    Some(15, 18)
    Some(12, 18)
    Some(15, 30)
    Some(12, 50)
    None
    None
] |> List.tryFind ((<||) (<>))