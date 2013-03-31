open System.Collections.Generic

type Direction = N | E | S | W with
    member this.ID = match this with N -> 0uy | E -> 1uy | S -> 2uy | W -> 3uy
    static member createFromID = function 0uy -> N | 1uy -> E | 2uy -> S | 3uy -> W | _ -> invalidArg "ID" "不正なIDです。"
    static member turn = function N -> E | E -> S | S -> W | W -> N

type Statues = { N : Direction; E : Direction; S : Direction; W : Direction } with
    static member isClear x =
        x .N = S && x .E = W && x .S = N && x .W = E
    member this.ID =
        this.N.ID <<< 6 ||| this.E.ID <<< 4 ||| this.S.ID <<< 2 ||| this.W.ID
    static member createFromID id =
        let create shift = id >>> shift &&& 3uy |> Direction.createFromID
        { N = create 6; E = create 4; S = create 2; W = create 0 }

let solveBaroqueTowerPuzzle statues =
    let getTurns statues =
        let turned = { N = Direction.turn statues.N; E = Direction.turn statues.E; S = Direction.turn statues.S; W = Direction.turn statues.W }
        let turn = function
            | 0 -> { turned with N = statues.N }
            | 1 -> { turned with E = statues.E }
            | 2 -> { turned with S = statues.S }
            | 3 -> { turned with W = statues.W }
            | _ -> invalidArg "buttonID" "不正なボタンIDです。"
        [for i in 0 .. 3 -> i, turn i]

    let isFirst =
        let memo = HashSet(HashIdentity.Structural)
        fun (statues : Statues) -> memo.Add statues

    let rec solve statuesList =
        statuesList |> List.tryFind (fst >> Statues.isClear) |> function
        | Some(_, result) -> List.rev result
        | None ->
            [for (s, r) in statuesList do
                if isFirst s then yield! s |> getTurns |> List.map (fun (i, s) -> s, i::r)]
            |> solve

    solve [statues, []]

solveBaroqueTowerPuzzle { N = W; E = S; S = S; W = W }
|> printfn "%A"
