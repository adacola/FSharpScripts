(*
赤または白の帽子をかぶった囚人が同じ方向を向いて一列に立っており、ある囚人からは前の囚人全員の帽子の色が見えるが、自分および後ろの囚人の帽子の色は見えない。
一番後ろの囚人がまず自分の帽子の色を答え、帽子の色が間違っていた場合は処刑される。次にその前の囚人が答え…という風に全員が自分の帽子の色を答える。
また、帽子の色を答える際に「赤」または「白」以外の言葉を発してはならないし、ジェスチャーなどの行動を行うこともできない。
なお、事前に囚人全員でどのように答えるかを相談してもよいものとする。
最も囚人全員の生存率が高くなるにはどのようにすればよいか？
*)

open System

let random = Random()

/// 帽子の色は白か赤の2種類。
type Hat = White | Red with
    /// 0の場合は白、1の場合は赤の帽子を作成。
    static member Create = function 0 -> White | _ -> Red
    /// 帽子が白の場合は0を、赤の場合は1を返す。
    member this.ToInt = match this with White -> 0 | Red -> 1
    /// 帽子が白の場合は赤の帽子を、赤の場合は白の帽子を返す。
    member this.Another = match this with White -> Red | Red -> White
    /// 文字列化
    override this.ToString() = match this with White -> "白" | Red -> "赤"

/// 囚人についてのクラス。
type PrisonerWareingHat(prisonerCount) =
    let expectHats = [for _ in 1 .. prisonerCount -> random.Next(2) |> Hat.Create]

    /// 囚人の人数
    member this.Count = prisonerCount
    /// 答えが実際と合っているかどうかを判定する。
    member this.Judge actualHats =
        actualHats |> Seq.zip <| expectHats
        |> Seq.map ((<||) (=)) |> Seq.toList
    /// 何番目の囚人かを指定すると、その囚人から見える帽子のリストを返す。
    member this.GetVisibleHats index = expectHats |> Seq.skip index |> Seq.toList
    /// 文字列化
    override this.ToString() = expectHats |> Seq.map string |> String.concat "," |> sprintf "正解 : %s"

/// 指定された解答候補が解答として適切かどうかを判定する。
/// prisonerCount : 囚人の人数
/// tell : 問題の解答候補となる関数。関数の引数は順に、囚人の人数、今の囚人から見える帽子のリスト、今までに囚人が答えた帽子のリスト。戻り値は今の囚人が答える帽子。
/// 戻り値 : 問題の解答候補が解答として正しければtrue
let judge prisonerCount (tell : int -> Hat list -> Hat list -> Hat) =
    if prisonerCount < 2 then invalidArg "prisonerCount" "囚人の人数には2以上を指定してください"
    let play() =
        let prisoners = PrisonerWareingHat(prisonerCount)
        printfn "%O" prisoners
        let actualHats =
            [] |> Seq.unfold (fun answers ->
                let index = answers.Length + 1
                let answer = tell prisoners.Count (prisoners.GetVisibleHats index) (List.rev answers)
                Some(answer, answer::answers))
            |> Seq.take prisonerCount |> Seq.toList
        actualHats |> Seq.map string |> String.concat "," |> printfn "解答 : %s\n"
        let results = prisoners.Judge actualHats
        results |> List.tail |> List.forall id
    let result = Seq.init 5 (fun _ -> play()) |> Seq.forall id
    printfn "%s" <| if result then "正解！" else "間違い"
    printfn ""

/// 問題の解答。
/// 最初の囚人は、見えている赤の帽子の人数が奇数なら赤、偶数なら白と答える。(最初の囚人の答えが実際の帽子の色と合っているかどうかはランダムとなる)
/// 2人目以降の囚人は、(自分から見える赤の帽子の個数 + 前までの囚人が答えた赤の帽子の個数)が奇数の場合、最初の囚人の答えと逆の色を答える。偶数の場合、最初の囚人の答えと同じ色を答える。
/// こうすることで、2人目以降の囚人の帽子の色を確実に当てることができる。
let solve prisonerCount (visibleHats : Hat list) =
    let sum hats = Seq.sumBy (fun (hat : Hat) -> hat.ToInt) hats % 2
    function
    | [] -> visibleHats |> sum |> Hat.Create
    | (first : Hat)::rest -> if (sum visibleHats + sum rest) % 2 = 0 then first else first.Another

// 囚人の人数は10人とする
let prisonerCount = 10
printfn "%s" "全員必ず赤と答える戦略"
judge prisonerCount (fun _ _ _ -> Red)
printfn "%s" "全員必ず白と答える戦略"
judge prisonerCount (fun _ _ _ -> White)
printfn "%s" "全員ランダムに答える戦略"
judge prisonerCount (fun _ _ _ -> random.Next 2 |> Hat.Create)
printfn "%s" "奇数番目の人は次の人の色を答え、偶数番目の人は前の人の答えをそのまま答える戦略(生存率50%以上)"
judge prisonerCount (fun _ v a -> if a.Length % 2 = 0 then List.head v else a |> List.rev |> List.head)
printfn "%s" "正解の戦略(最初の囚人以外は確実に生存)"
judge prisonerCount solve
