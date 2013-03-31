let d'hondt seatCount votes =
    if seatCount < 0 then seatCount |> sprintf "議席数に負の値が指定されました : %d" |> invalidArg "seatCount"
    elif Map.isEmpty votes then invalidArg "votes" "得票数に空マップが指定されました"

    let votes = votes |> Map.map (fun _ -> decimal)
    let initialResult = votes |> Map.map (fun _ _ -> 0)
    (initialResult, seq { 1 .. seatCount }) ||> Seq.fold (fun result _ ->
        let inline calc party vote = vote / (Map.find party result |> decimal |> (+) 1M)
        let nextParty = votes |> Map.toSeq |> Seq.maxBy ((<||) calc) |> fst
        result |> Map.add nextParty (Map.find nextParty result + 1))
