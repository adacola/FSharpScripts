(*
　Figのような盤と十円玉3個，百円玉1個を使ってふたりで遊ぶ 
ゲームを紹介する。遊び方は以下の通り。

　①図のH，K，Jに十円玉を一個ずつ置き，猟犬とする
　②百円玉はウサギで，猟犬のいないところならどこに置いても
　　よい
　③先手は猟犬
　④線に沿って隣が空いていればその○に，猟犬のうちの一匹が
　　移動する。ただし、バックはできない(たとえば図のFから
　　B，C，D，E，Gには行けるが，H，I，Jには行けない)
　⑤ウサギも隣が空いていれば線に沿って移動する。方向は自由
　⑥以下④と⑤を交互に続ける

　最終的にウサギを動けなくしたら猟犬の勝ち，猟犬の包囲網を
通り抜けたら(犬はバックができないのだからウサギが犬のうし 
ろに回り込めば)ウサギの勝ちである。なお，千日手(同じ場面の
繰り返し)になった場合は猟犬側にその解消義務がある。

　さてこのゲーム，実は猟犬側に必勝法がある。猟犬をコンピュ
ータが担当し，人と対戦する必勝プログラムを作ってほしい。対
戦の表示方法はご自由にどうぞ。

　　　　　　Ａ
　　　　　／│＼
　　　　Ｂ─Ｃ─Ｄ
　　　　│＼│／│
　　　　Ｅ─Ｆ─Ｇ
　　　　│／│＼│
　　　　Ｈ─Ｉ─Ｊ
　　　　　＼│／
　　　　　　Ｋ
　　Fig.４ ウサギと犬
*)

open System
open System.Collections.Generic

/// 探索結果
type SearchResult =
    /// 猟犬の勝利確定。 (n, h) : あとnターンで勝利確定。次の状態のハッシュはh。
    | Win of int * int
    /// 未確定。
    | Unknown
    /// 猟犬の敗北。
    | Loss

/// ゲームを実行する。
let play =
    /// リストからr個の要素を取り出したリストの全組み合わせを取得。
    let combination r ls =
        let rec combination' r ls =
            match List.length ls, r with
            | _, 0 -> Seq.empty
            | n, r when n < r -> Seq.empty
            | n, r when n = r -> Seq.singleton ls
            | _, r when r = 1 -> Seq.map (fun x -> [x]) ls
            | n, r ->
                Seq.append
                    (combination' (r - 1) (List.tail ls) |> Seq.map (fun x -> List.head ls::x))
                    (combination' r (List.tail ls))
        ls |> Seq.toList |> combination' r

    /// 盤面の全マス。
    let field = ['A' .. 'K']

    /// 各マスの最前方からの距離。
    let fieldDistanceFromFront = [0; 1; 1; 1; 2; 2; 2; 3; 3; 3; 4] |> List.zip field |> dict

    /// fieldRabbitMovable : 各マスと対応するウサギの移動可能マスの辞書。
    /// fieldDogMovable : 各マスと対応する猟犬の移動可能マスの辞書。
    let fieldRabbitMovable, fieldDogMovable =
        let neighbors =
            [
                set "BCD"
                set "ACEF"
                set "ABDF"
                set "ACFG"
                set "BFH"
                set "BCDEGHIJ"
                set "DFJ"
                set "EFIK"
                set "FHJK"
                set "FGIK"
                set "HIJ"
            ] |> List.zip field
        dict neighbors,
        neighbors |> List.map (fun (k, v) -> k, v |> Set.filter (fun p -> fieldDistanceFromFront.[p] <= fieldDistanceFromFront.[k]))
        |> dict

    /// ウサギの移動後の盤面パターン取得する。
    let moveRabbit (rabbit, dogs) =
        fieldRabbitMovable.[rabbit] |> Seq.filter (fun r -> dogs |> Set.forall ((<>) r))
        |> Seq.map (fun rabbit -> rabbit, dogs)

    /// 猟犬の移動後の盤面パターン取得する。
    let moveDog (rabbit, dogs) =
        dogs |> Seq.collect (fun dog ->
            let otherDogs = dogs |> Set.remove dog
            let newDogs = fieldDogMovable.[dog] |> Seq.filter (fun d -> d <> rabbit && otherDogs |> Seq.forall ((<>) d))
            newDogs |> Seq.map (fun newDog -> otherDogs |> Set.add newDog))
        |> Seq.distinct |> Seq.map (fun dogs -> rabbit, dogs)

    /// 盤面の全パターン。
    let allPatterns =
        [for rabbit in field do
            yield! field |> List.filter ((<>) rabbit) |> combination 3 |> Seq.map (fun xs -> rabbit, set xs)]

    /// 盤面の状態をハッシュに変換する。
    let stateToHash =
        let memo = allPatterns |> Seq.mapi (fun i p -> p, i) |> dict
        fun state -> memo.[state]

    /// ハッシュを盤面の状態に変換する。
    let hashToState =
        let memo = allPatterns |> Seq.mapi (fun i p -> i, p) |> dict
        fun hash -> memo.[hash]

    /// 猟犬の勝利となる全盤面パターン。
    let winPatterns =
        fieldRabbitMovable |> Seq.choose (function
            | KeyValue(k, _) when k = 'K' -> None
            | KeyValue(k, v) when v.Count = 3 -> Some(stateToHash (k, v))
            | _ -> None)
        |> Set.ofSeq

    /// 次のターンに猟犬の勝利となる全盤面ハッシュ。
    /// (s, w) : sは次のターンに猟犬の勝利となる盤面のハッシュ。wは最終的な勝利盤面のハッシュ。
    let prevWinPatterns =
        allPatterns
        |> List.choose (fun state ->
            let stateHash = stateToHash state
            state |> moveDog |> Seq.map stateToHash |> Seq.tryPick (fun next ->
                winPatterns |> Seq.tryFind ((=) next) |> Option.map (fun win -> stateHash, win)))

    /// 猟犬の敗北（＝ウサギの勝利）となる全盤面パターン。
    let losePatterns =
        allPatterns |> Seq.filter (fun (rabbit, dogs) ->
            dogs |> Set.forall (fun dog -> fieldDistanceFromFront.[dog] < fieldDistanceFromFront.[rabbit]))
        |> Seq.map stateToHash |> Set.ofSeq

    /// すべての盤面について勝利手順を探索する。
    let searchResult =
        /// 盤面の探索結果の初期状態。
        let initialSearchResult =
            allPatterns |> List.map (stateToHash >> function
                | s when Set.contains s winPatterns -> s, Win(-1, s)
                | s when Set.contains s losePatterns -> s, Loss
                | s ->
                    match prevWinPatterns |> List.tryFind (fst >> (=) s) with
                    | Some(_, w) -> s, Win(0, w)
                    | None -> s, Unknown)
            |> dict
        let searchResult = Dictionary(initialSearchResult)
        // 探索結果が未確定の局面について、まず猟犬を動かし、次にウサギを動かして、次の局面が確定した場合はその状態を採用する。
        // 探索結果が変化しなくなるまで探索を続行。探索終了時点で、ゲームの初期配置については猟犬の勝利が確定しているはず。
        let rec search() =
            searchResult |> Seq.choose (function
                | KeyValue(state, Unknown) ->
                    match state |> hashToState |> moveDog |> Seq.toList with
                    | [] -> Some(state, Loss)   // 猟犬が身動きがとれなくなるパターンがあるので、その場合はウサギの勝ちとする。
                    | dogs ->
                        // まず猟犬の動き方の全パターンそれぞれについて、ウサギをできる限りウサギの勝利に近づくように動かす。
                        // （ウサギの勝利確定への動き方があればそれを採用し、なければ未確定状態への動き方を採用する）
                        // その結果を比較して、猟犬はできる限り猟犬の勝利に近づくような動き方を採用する。
                        // （猟犬の勝利確定への動き方があればそれを採用（なるべく最短で勝利する場所を選ぶ）し、なければ未確定状態への動き方を採用する）
                        let next =
                            dogs |> Seq.map (moveRabbit >> Seq.map stateToHash >> Seq.maxBy (fun n -> searchResult.[n]))
                            |> Seq.minBy (fun n -> searchResult.[n])
                        match searchResult.[next] with
                        | Win(n, _) -> Some(state, Win(n + 1, next))
                        | Loss -> Some(state, Loss)
                        | Unknown -> None
                | _ -> None)
            |> Seq.toList |> function
                | [] -> searchResult
                | changed ->
                    changed |> List.iter (fun (state, result) -> searchResult.[state] <- result)
                    search()
        search()

    /// 人間と対戦を行うメイン部分。
    let main() =
        /// 表示用のフィールド名。
        let fieldString = ['Ａ' .. 'Ｋ']
        /// フィールド名を表示用に変換する。
        let fieldToPrint = List.zip field fieldString |> dict
        /// 盤面を表示する。
        /// state : Some(s)の場合、sのハッシュ番号に対応する盤面の状態を出力。Noneの場合、コマを表示せずマスの名前のみ出力する。
        let print state =
            let fields =
                (match state with
                 | Some s ->
                    let rabbit, dogs = hashToState s
                    field |> List.map (fun x -> if x = rabbit then '兎' elif Set.contains x dogs then '犬' else fieldToPrint.[x])
                 | None -> fieldString)
                |> Seq.cast<obj> |> Seq.toArray
            Console.WriteLine("　　{0}\n　／│＼\n{1}─{2}─{3}\n│＼│／│\n{4}─{5}─{6}\n│／│＼│\n{7}─{8}─{9}\n　＼│／\n　　{10}", fields)
        /// ウサギの移動場所を入力する。
        let rec input message condition =
            printfn "%s" message
            match Console.ReadLine() with
            | x when x.Length = 1 && condition (x.ToUpper()).[0] -> (x.ToUpper()).[0]
            | _ -> input message condition
        /// ゲーム開始時にウサギをどこに初期配置するかを入力し、初期状態の盤面ハッシュを返す。
        let start() =
            print None
            let rabbit = input "ウサギの初期配置場所を H,J,K 以外から選んでください。" (fun x -> 'I'::['A' .. 'G'] |> List.exists ((=) x))
            (rabbit, set "HJK") |> stateToHash
        /// ウサギをどこに移動するかを入力し、ウサギ移動後の盤面ハッシュを返す。
        let rec playRabbit state =
            print (Some state)
            let rabbit, dogs = state |> hashToState
            let nextPatterns = (rabbit, dogs) |> moveRabbit |> Set.ofSeq
            let rabbit = input "ウサギの移動場所を選んでください。" (fun x -> Set.contains (x, dogs) nextPatterns)
            (rabbit, dogs) |> stateToHash |> playDog
        /// 猟犬を移動させる。
        and playDog state =
            match searchResult.[state] with
            | Win(0, h) ->
                print (Some h)
                printfn "猟犬の勝ちです。"
            | Loss ->
                print (Some state)
                printfn "ウサギの勝ちです！"
            | Win(_, h) ->
                let rabbit = hashToState state |> fst
                let dogs = hashToState h |> snd
                (rabbit, dogs) |> stateToHash |> playRabbit
            | _ -> failwith "プログラムのロジックエラー"
        start() |> playDog
    main

play()
