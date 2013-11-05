(*
http://kupc2013.contest.atcoder.jp/tasks/kupc2013_e

グラフを作り、移動する際にはゴールまでたどり着く可能性のあるマスへ移動する。
グラフを作る際は、各サイコロの目の+-両方向について移動先の指示分移動した先へのリンクをエッジとする。
*)

/// グラフを作成します。
/// moves : サイコロの各目に対応する移動マス数の配列。インデックス増加方向に進むのを正とします。
/// start : スタート地点のインデックス(0始まり)。
/// goal : ゴール地点のインデックス(0始まり)。start <> goal である必要があります。
/// field : すごろくのマスの情報。インデックス増加方向に進むのを正とします。field.[start]とfield.[goal]は0である必要があります。
/// 戻り値 : グラフ。「始点と終点のリスト」のマップ。
let createGraph (moves : int[]) start goal (field : int[]) =
    assert (moves.Length = 6 && 0 <= start && start < field.Length && 0 <= goal && goal < field.Length && start <> goal && field.[start] = 0 && field.[goal] = 0)
    let (|InField|_|) x = if 0 <= x && x < field.Length then Some x else None
    let moves = moves |> Seq.collect (fun move -> [move; -move]) |> Seq.distinct |> Seq.toList
    let rec createGraph edges = function
        | [] -> edges
        | positions ->
            let edges' =
                positions |> Seq.collect (fun pos ->
                    moves |> List.choose ((+) pos >> function
                        | InField pos' -> Some(pos, pos' + field.[pos'])
                        | _ -> None))
                |> Seq.distinct |> Seq.toList
            let positions' =
                edges' |> Seq.choose (fun (_, pos') -> if pos' = goal || Map.containsKey pos' edges || List.exists ((=) pos') positions then None else Some pos')
                |> Seq.distinct |> Seq.toList
            let nextEdges =
                edges' |> Seq.groupBy fst |> Seq.map (fun (k, vs) -> k, vs |> Seq.map snd |> Seq.toList)
                |> Seq.fold (fun es (k, v) -> Map.add k v es) edges
            createGraph nextEdges positions'
    createGraph Map.empty [start]

/// ゴールに到達可能な点を抽出します。
/// goal : ゴール地点のインデックス(0始まり)。
/// graph : グラフ。「始点と終点のリスト」のマップ。
/// 戻り値 : ゴールに到達可能な点(0始まり)とゴールまでの最短ステップ数のマップ。ゴールも含みます。
let filterReachableGoal goal graph =
    let graph = graph |> Map.toList
    let rec filterReachable step result = function
        | [] -> result
        | positions ->
            let positions' =
                positions |> List.collect (fun pos ->
                    graph |> List.choose (fun (k, vs) -> if vs |> List.exists ((=) pos) then Some k else None))
                |> List.filter (fun pos -> result |> Map.containsKey pos |> not)
            let result' = (result, positions') ||> List.fold (fun r p -> Map.add p step r)
            filterReachable (step + 1) result' positions'
    filterReachable 1 (Map.ofList [goal, 0]) [goal]

/// 次のマスに移動します。
/// position : 現在のマス(0始まり)。
/// step : 移動数。
/// reachable : ゴールに到達可能な点とゴールまでの最短ステップ数のマップ。
/// field : マスの情報。
/// 戻り値 : 移動先のマス(0始まり)。
let move position moving reachable (field : int[]) =
    let nexts = [moving; -moving] |> List.map ((+) position) |> List.filter (fun x -> 0 <= x && x < field.Length)
    let positions = position::(nexts |> List.map (fun x -> x + field.[x]))
    positions |> List.choose (fun pos -> Map.tryFind pos reachable |> Option.map (fun x -> x, pos)) |> List.sort |> List.head |> snd

open System

/// ランダムに1～移動数の最大値を返す関数を返します。
/// moves : 移動数の配列。
/// 戻り値 : ランダムに1～移動数の最大値のどれかを返す関数。
let random moves =
    let len = Array.length moves
    let rand = Random()
    fun () -> rand.Next len + 1

/// すごろくをプレイします。
/// random : ランダムな値を発行する関数。
/// moves : サイコロの各目に対応する移動マス数の配列。インデックス増加方向に進むのを正とします。
/// start : スタート地点のインデックス(0始まり)。
/// goal : ゴール地点のインデックス(0始まり)。start <> goal である必要があります。
/// field : すごろくのマスの情報。インデックス増加方向に進むのを正とします。field.[start]とfield.[goal]は0である必要があります。
/// 戻り値 : 各ステップごとの、(移動後のマス(0始まり), ランダムのマス数) のリスト
let play random (moves : int[]) start goal (field : int[]) =
    let graph = createGraph moves start goal field
    let reachable = filterReachableGoal goal graph
    let rand = random moves
    let rec play step result = function
        | _ when step > 3000 -> None
        | position when position = goal -> result |> List.rev |> Some
        | position ->
            let index = rand()
            let moving = moves.[index - 1]
            let next = move position moving reachable field
            let result' = (next, moving)::result
            play (step + 1) result' next
    play 0 [] start

let main random =
    Console.ReadLine() |> ignore
    let moves = Console.ReadLine().Split([|' '|]) |> Array.map int
    let [|start; goal|] = Console.ReadLine().Split([|' '|]) |> Array.map int
    let field = Console.ReadLine().Split([|' '|]) |> Array.map int
    let result = play random moves (start - 1) (goal - 1) field
    printfn "%A" result
