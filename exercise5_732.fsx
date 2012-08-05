(*
文字列が与えられたとき、この文字列を出力するBrainf*ckプログラムを生成せよ。
*)
let toBF (input : string) =
    let diff x y =
        let c, s = if x - y < 128uy then x - y, "-" else y - x, "+"
        String.replicate (int c) s + "."
    let bytes = System.Text.Encoding.UTF8.GetBytes input
    let prevs = bytes |> Seq.append [0uy] |> Seq.pairwise |> Seq.map ((<||) diff)
    let zeros = bytes |> Seq.map (diff 0uy >> (+) ">")
    Seq.zip prevs zeros |> Seq.map (fun (prev, zero) -> [prev; zero] |> Seq.minBy String.length)
    |> String.concat ""
