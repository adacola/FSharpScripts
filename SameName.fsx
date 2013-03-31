let groupSameName sameNames =
    let splitNames (sameName : string) = let names = sameName.Split([|'='|]) in names.[0], names.[1]
    let joinNames = Seq.map (String.concat "=")

    let register dic (name1, name2) =
        let getName name =
            Map.tryFind name >> function None -> Set.singleton name | Some names -> names
        let nameSet = getName name1 dic + getName name2 dic
        (dic, nameSet) ||> Set.fold (fun dic name -> Map.add name nameSet dic)

    let sameNamePairs = sameNames |> Seq.map splitNames
    (Map.empty, sameNamePairs) ||> Seq.fold register
    |> Map.toSeq |> Seq.map snd |> Seq.distinct
    |> Seq.sort |> joinNames

let test input expected =
    let splitLine (x : string) = x.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
    splitLine input |> groupSameName |> Seq.toArray |> (=) (splitLine expected)
