// １～９までの連続する数字の間に和・差・積の演算子(+, -, *)を適当に入れて、
// 式の値がちょうど 100 となる例を挙げよ

type Op = Add | Sub | Mul

let komachi() =
    let rec makeOpss opss = function
    | 0 -> opss
    | n ->
        opss |> Seq.collect (fun ops ->
            [Add; Sub; Mul] |> List.map (fun op -> seq { yield op; yield! ops }))
        |> makeOpss <| n - 1
    makeOpss [[]] 8 |> Seq.choose (fun ops ->
        ((1, []), Seq.zip ops [9 .. -1 .. 2])
        ||> Seq.fold (fun (m, nos) -> function Mul, i -> (m * i, nos) | op, i -> (1, (op, m * i)::nos))
        ||> Seq.fold (fun r -> function Add, i -> r + i | Sub, i -> r - i | Mul, _ -> failwith "ロジックエラー")
        |> function
        | 100 ->
            ops |> Seq.map (function Add -> " + " | Sub -> " - " | Mul -> " * ")
            |> Seq.append (Seq.singleton "") |> Seq.toList |> List.rev |> List.zip [1 .. 9]
            |> List.map ((<||) (sprintf "%d%s")) |> String.concat "" |> sprintf "%s = 100" |> Some
        | _ -> None)

komachi() |> Seq.take 10            
