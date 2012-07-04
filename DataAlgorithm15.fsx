// 解を列挙する関数
let solve xBlockNum yBlockNum =
    let [xLen; yLen] = [xBlockNum; yBlockNum] |> List.map (fun n -> n * 2 - 1)
    let markingNum = xBlockNum * yBlockNum * 2 - 1
    let directions = [1, 0; 0, 1; -1, 0; 0, -1]
    let memo = System.Collections.Generic.HashSet(HashIdentity.Structural)

    let getNext markedSet candidates =
        candidates |> Seq.map (fun ((x, y), (dx, dy)) ->
            let nx, ny = x + dx, y + dy
            let newMarked = markedSet |> Set.add (x, y) |> Set.add (nx, ny)
            directions |> List.choose (fun (dx, dy) ->
                if 1 <= nx + dx && nx + dx <= xLen && 1 <= ny + dy && ny + dy <= yLen
                then Some((nx + dx, ny + dy), (dx, dy)) else None)
            |> Seq.append candidates
            |> Seq.filter (fun ((x, y), (dx, dy)) -> Set.contains (x + dx, y + dy) newMarked |> not)
            |> fun nextCandidates -> newMarked, nextCandidates)

    let rec solve markedSet candidates = seq {
        if memo.Add markedSet then
            if Set.count markedSet = markingNum then yield markedSet
            else yield! getNext markedSet candidates |> Seq.collect ((<||) solve) }

    solve (set [1, 1]) [(2, 1), (1, 0); (1, 2), (0, 1)]
    |> Seq.map (fun result ->
        [for y in 0 .. yLen + 1 ->
            [for x in 0 .. xLen + 1 -> if Set.contains (x, y) result then "*" else "-"]])

// 解を10個表示
solve 4 4 |> Seq.take 10 |> Seq.iter (fun result ->
    result |> Seq.iter (String.concat "" >> printfn "%s"); printfn "")
