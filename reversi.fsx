open System

type Cell = Space | Black | White | Outside with
    static member isStone = function Black | White -> true | _ -> false
    static member reverse = function Black -> Some White | White -> Some Black | _ -> None
    override this.ToString() = match this with Space -> "-" | Black -> "o" | White -> "x" | Outside -> ""

let reversi() =
    let rec input message defaultResult func =
        printfn "%s %s" message (match defaultResult with Some d -> sprintf "[%O]" d | _ -> "")
        match Console.ReadLine(), defaultResult with
        | "", Some d -> d
        | inp, _ -> match func inp with Some result -> result | None -> input message defaultResult func

    let field =
        Array.init 10 (fun y -> Array.init 10 (fun x ->
            match x, y with
            | 0, _ | 9, _ | _, 0 | _, 9 -> Outside
            | 4, 4 | 5, 5 -> White
            | 4, 5 | 5, 4 -> Black
            | _ -> Space))

    let directions = [for x in -1 .. 1 do for y in -1 .. 1 -> x, y] |> List.filter ((<>) (0, 0))

    let printField (field : Cell[][]) =
        field |> Array.iter (fun line -> 
            line |> Array.iter (fun cell -> cell.ToString() |> printf "%s")
            printfn "")

    let reverse cell (field : Cell[][]) (x, y) =
        if field.[y].[x] <> Space then invalidArg "(x, y)" "指定されたマスが空白ではありません"

        let reverseLine ((x, y) as pos) cell (field : Cell[][]) (dx, dy) =
            let rec checkLine result (x, y) =
                let x', y' = x + dx, y + dy
                match Cell.reverse field.[y'].[x'] with
                | None -> []
                | Some c when c = cell -> checkLine ((x', y')::result) (x', y')
                | _ -> result
            checkLine [] (x, y)
                
        directions |> List.collect (reverseLine (x, y) cell field)
        |> function
            | [] -> []
            | xs -> (x, y)::xs

    let canPut pos cell field =
        reverse cell field pos |> List.isEmpty |> not

    let canPutSomewhere cell (field : Cell[][]) =
        seq { for y in 1 .. 8 do for x in 1 .. 8 do if field.[y].[x] = Space then yield x, y }
        |> Seq.exists (fun pos -> canPut pos cell field)

    let continuesGame field = [Black; White] |> List.exists (fun cell -> canPutSomewhere cell field)

    let putStone cell (field : Cell[][]) =
        let condition (inp : string) =
            inp.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> function
                | [|x; y|] ->
                    match Int32.TryParse x, Int32.TryParse y with
                    | (true, x), (true, y) when 1 <= x && x <= 8 && 1 <= y && y <= 8 ->
                        if field.[y].[x] <> Space then printfn "空白のマスを指定してください。"; None
                        elif canPut (x, y) cell field |> not then printfn "そのマスには置けません。"; None
                        else Some(x, y)
                    | _ -> None
                | _ -> None
        printField field
        input "「x y」の形式で石を置く場所を指定してください。" None condition

    let printTurn = printfn "\n%O の番です。"

    let printPass() = printfn "置く場所がありませんので、パスします。"

    let rec turn putStone printTurn printPass passed cell field =
        let turn' = turn putStone printTurn printPass
        printTurn cell
        if canPutSomewhere cell field then
            let reverseTarget = putStone cell field |> reverse cell field |> set
            field |> Array.mapi (fun y -> Array.mapi (fun x c -> if Set.contains (x, y) reverseTarget then cell else c))
            |> turn' false (Cell.reverse cell |> Option.get)
        else
            printPass()
            if passed then field else turn' true (Cell.reverse cell |> Option.get) field

    let judgeResult field =
        let counts = field |> Seq.concat |> Seq.filter Cell.isStone |> Seq.countBy id |> Map.ofSeq
        let getCount cell = defaultArg (Map.tryFind cell counts) 0
        let cellAndCounts = [for cell in [Black; White] -> cell, getCount cell]
        let maxCount = cellAndCounts |> Seq.map snd |> Seq.max
        let winners = cellAndCounts |> Seq.filter (snd >> (=) maxCount) |> Seq.map fst |> Seq.toList
        cellAndCounts, winners
    
    let printResult cellAndCounts winners =
        cellAndCounts |> List.iter ((<||) (printfn "%O の石の数 : %d 個"))
        match winners with
        | winner::[] -> winner.ToString() |> printfn "%s の勝ち！"
        | _ -> printfn "引き分け！"

    turn putStone printTurn printPass false Black field |> judgeResult ||> printResult

reversi()
