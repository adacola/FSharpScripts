// http://www.moon.sannet.ne.jp/okahisa/haskell-sort/RB-treesort.html を参考にF#で赤黒木を実装。

module Wasabi.Tree.RedBlackTree

open System.Collections
open System.Collections.Generic

/// ノードの色
type Colors =
    /// 赤
    | R
    /// 黒
    | B

/// ツリー
type Tree<'a when 'a : comparison> =
    /// 葉
    | Leaf
    /// ノード。 color : ノードの色。 data : ノードに登録されているデータ。 left : 左部分木。 right : 右部分木。
    | Node of color : Colors * data : 'a * left : 'a Tree * right : 'a Tree
with
    member private this.enumerator = 
        match this with
        | Leaf -> Seq.empty.GetEnumerator()
        | Node(_, data, left, right) -> (seq { yield! left; yield data; yield! right }).GetEnumerator()
    interface IEnumerable<'a> with
        member this.GetEnumerator() = this.enumerator
    interface IEnumerable with
        member this.GetEnumerator() = this.enumerator :> IEnumerator

/// 空のノード
let empty = Leaf

/// ツリーが空かどうか判定します。
let isEmpty = function Leaf -> true | Node(_, _, _, _) -> false

/// ツリーに新しいデータを追加します。
let add data tree =
    /// ノードを再構成します。
    let mkNode = function
        | B, u, Node(R, v, Node(R, w, t1, t2), t3), t4 -> Node(R, v, Node(B, w, t1, t2), Node(B, u, t3, t4)) // パターンLL
        | B, u, Node(R, v, t1, Node(R, w, t2, t3)), t4 -> Node(R, w, Node(B, v, t1, t2), Node(B, u, t3, t4)) // パターンLR
        | B, u, t1, Node(R, v, Node(R, w, t2, t3), t4) -> Node(R, w, Node(B, u, t1, t2), Node(B, v, t3, t4)) // パターンRL
        | B, u, t1, Node(R, v, t2, Node(R, w, t3, t4)) -> Node(R, v, Node(B, u, t1, t2), Node(B, w, t3, t4)) // パターンRR
        | color, u, t1, t2 -> Node(color, u, t1, t2)

    /// ノードにデータを追加します。
    let rec add = function
        | Leaf -> Node(R, data, Leaf, Leaf)
        | Node(color, d, left, right) as node ->
            match compare data d |> sign with
            | -1 -> mkNode (color, d, add left, right)
            | 1  -> mkNode (color, d, left, add right)
            | _  -> node

    match add tree with
    | Node(_, d, left, right) -> Node(B, d, left, right) // 根は必ず黒
    | Leaf -> failwith "プログラムのバグです" // データを追加したので葉はありえない
    
/// ツリーからデータを削除します。
let remove data tree =
    /// 左部分木の黒ノード個数が減った場合の修正パターン
    let rec patmatL = function
        | rb, u, t1, Node(B, v, Node(R, w, t2, t3), t4) -> false, Node(rb, w, Node(B, u, t1, t2), Node(B, v, t3, t4))
        | rb, u, t1, Node(B, v, t2, Node(R, w, t3, t4)) -> false, Node(rb, v, Node(B, u, t1, t2), Node(B, w, t3, t4))
        | rb, u, t1, Node(B, v, t2, t3) -> rb = B, Node(B, u, t1, Node(R, v, t2, t3))
        | B, u, t1, Node(R, v, t2, t3) -> false, Node(B, v, patmatL (R, u, t1, t2) |> snd, t3)
        | _ -> failwith "プログラムのバグです"

    /// 右部分木の黒ノード個数が減った場合の修正パターン
    let rec patmatR = function
        | rb, u, Node(B, v, t1, Node(R, w, t2, t3)), t4 -> false, Node(rb, w, Node(B, v, t1, t2), Node(B, u, t3, t4))
        | rb, u, Node(B, v, Node(R, w, t1, t2), t3), t4 -> false, Node(rb, v, Node(B, w, t1, t2), Node(B, u, t3, t4))
        | rb, u, Node(B, v, t1, t2), t3 -> rb = B, Node(B, u, Node(R, v, t1, t2), t3)
        | B, u, Node(R, v, t1, t2), t3 -> false, Node(B, v, t1, patmatR (R, u, t2, t3) |> snd)
        | _ -> failwith "プログラムのバグです"

    /// 左部分木の最大値のノードを取得します。
    let rec leftMax = function
        | Node(color, d, left, Leaf) -> d, (color = B, left)
        | Node(color, d, left, right) ->
            match leftMax right with
            | lm, (true, rr) -> lm, patmatR (color, d, left, rr)
            | lm, (false, rr) -> lm, (false, Node(color, d, left, rr))
        | Leaf -> failwith "プログラムのバグです"

    /// dataを削除します。
    let rec remove = function
        | Leaf -> false, Leaf
        | Node(color, d, left, right) ->
            match compare data d |> sign with
            | -1 -> match remove left  with true, ll -> patmatL (color, d, ll, right) | false, ll -> false, Node(color, d, ll, right)
            | 1  -> match remove right with true, rr -> patmatR (color, d, left, rr)  | false, rr -> false, Node(color, d, left, rr)
            | _  ->
                if isEmpty left then color = B, right else
                match leftMax left with
                | lm, (true, ll) -> patmatL (color, lm, ll, right)
                | lm, (false, ll) -> false, Node(color, lm, ll, right)

    match remove tree |> snd with
    | Leaf -> Leaf
    | Node(_, d, left, right) -> Node(B, d, left, right)

/// 指定された値がツリーに含まれているかどうか判定します。
let rec contains data = function
    | Leaf -> false
    | Node(_, d, _, _) when d = data -> true
    | Node(_, d, left, right) -> contains data (if data < d then left else right) 

/// 性質テスト
let test() =
    let random = System.Random()
    let addTest count =
        printfn "要素数 %d のテスト開始" count
        let xs = [for _ in 1 .. count -> random.Next 5000]
        printfn "テストデータ抜粋 : %A" xs
        let expected = xs |> Seq.distinct |> Seq.sort |> Seq.toList
        let actual = (Leaf, xs) ||> List.fold (fun tree data -> add data tree) |> Seq.toList
        if actual <> expected then failwith "test failed."
        printfn "要素数 %d のテスト成功" count
    let removeTest count =
        printfn "要素数 %d から100個削除するテスト開始" count
        for _ in 1 .. 10 do
            let original = [for _ in 1 .. max count 100 -> random.Next 5000]
            let xs = original |> Seq.take count |> Seq.toList
            let removeDatum = original |> Seq.take 100 |> Seq.toList
            let expected = xs |> Seq.distinct |> Seq.filter (fun x -> List.forall ((<>) x) removeDatum) |> Seq.sort |> Seq.toList
            let tree = (Leaf, xs) ||> List.fold (fun tree data -> add data tree)
            let actual = (tree, removeDatum) ||> List.fold (fun tree data -> remove data tree) |> Seq.toList
            if actual <> expected then failwith "test failed."
        printfn "要素数 %d から100個削除するテスト成功" count
    let containsTest() =
        printfn "containsテスト開始"
        let xs = [|for _ in 1 .. 10000 -> random.Next 5000|]
        let tree = (Leaf, xs) ||> Array.fold (fun tree data -> add data tree)
        if contains -1 tree then failwith "test failed."
        for _ in 1 .. 10 do
            if contains xs.[random.Next xs.Length] tree |> not then failwith "test failed."
        printfn "containsテスト成功"
    let isEmptyTest() =
        printfn "isEmptyテスト開始"
        let xs = [for _ in 1 .. 10000 -> random.Next 5000]
        let actual = (Leaf, xs) ||> List.fold (fun tree data -> add data tree)
        if isEmpty actual then failwith "test failed."
        if Leaf |> add 1 |> isEmpty then failwith "test failed."
        if Leaf |> isEmpty |> not then failwith "test failed."
        printfn "isEmptyテスト成功"
        
    addTest 10000
    addTest 0
    addTest 1
    addTest 2
    removeTest 10000
    removeTest 0
    removeTest 1
    removeTest 100
    containsTest()
    isEmptyTest()
