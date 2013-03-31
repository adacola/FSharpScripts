open System.Collections.Generic

let buyCount totalCost items =
    let memo = Dictionary()
    let rec buyCount totalCost items =
        match memo.TryGetValue((totalCost, items)) with
        | true, result -> result
        | false, _ ->
            let result =
                match sign totalCost, items with
                | 0, _ -> Some []
                | -1, _ | _, [] -> None
                | _ , (name, cost)::rest->
                    [[name, cost], totalCost - cost; [], totalCost]
                    |> List.choose (fun (item, nextCost) -> buyCount nextCost rest |> Option.map (List.append item))
                    |> function [] -> None | results -> results |> List.maxBy List.length |> Some
            memo.Add((totalCost, items), result)
            result
    buyCount totalCost items

let items = [
    ("USP", 500); ("Glock", 400); ("P229", 600); ("DesertEagle", 650); ("FiveseveN", 750);
    ("Beretta", 800); ("M3", 1700); ("M1014", 3000); ("TMP", 1250); ("Mac10", 1400);
    ("MP5", 1500); ("UMP", 1700); ("P90", 2350); ("Galil", 2000); ("FAMAS", 2250);
    ("AK47", 2500); ("M4A1", 3100); ("AUG", 3500); ("SG552", 3500); ("Scout", 2750);
    ("SG550", 4200); ("G3SG1", 5000); ("AWP", 4750); ("M249", 5750); ("HEGrenade", 300);
    ("Flashbang", 200); ("SmokeGrenade", 300); ("Vest", 650); ("Helmet", 350); ("Shield", 2200);
    ("DefuseKit", 200); ("NightVision", 1250)]

//目標金額
let totalCost = 16000

buyCount totalCost items