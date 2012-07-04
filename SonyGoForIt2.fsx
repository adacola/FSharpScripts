open System

/// 問題を解く。
let solve x =
    /// ガンマ関数の対数。
    let loggamma x =
        let N = 8.
        let bernoullis = [ // ベルヌーイ数（B2から2刻み）
            1.0 / 6.0
            -1.0 / 30.0
            1.0 / 42.0
            -1.0 / 30.0
            5.0 / 66.0
            -691.0 / 2730.0
            7.0 / 6.0
            -3617.0 / 510.0
        ]

        let rec increment v = function
        | x when x < N -> increment (v * x) (x + 1.)
        | x -> v, x
        let v, x = increment 1. x
        let w = 1. / (x * x)
        List.zip bernoullis [2 .. 2 .. bernoullis.Length * 2] |> List.rev
        |> Seq.map (fun (b, n) -> b / float (n * (n - 1)))
        |> Seq.reduce (fun b1 b2 -> b1 * w + b2)
        |> fun y -> y / x + 0.5 * log (2. * Math.PI) - log v - x + (x - 0.5) * log x

    /// ガンマ関数。
    let gamma x =
        if x < 0. then Math.PI / (sin (Math.PI * x) * exp (loggamma(1. - x)))
        else exp (loggamma x)

    gamma (x + 1.) |> printfn "%g! = %.12g" x

printfn "i)"
[0 .. 10] |> List.iter (float >> solve)
printfn "ii)"
[0. .. 0.1 .. 10.] |> List.iter solve
printfn "iii)"
[-1.9 .. 0.1 .. -1.09] |> List.iter solve
