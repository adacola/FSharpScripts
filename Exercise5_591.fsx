// どの桁の数字も右隣の桁の数字よりも大きい2桁以上の10進数をすべて求めよ。

[for i in 9L .. -1L .. 0L -> [i]]
|> Seq.unfold
    (List.collect (fun (x::xs) -> [for i in 9L .. -1L .. x + 1L -> i::x::xs])
     >> function [] -> None | r -> Some(r, r))
|> Seq.concat |> Seq.map (Seq.reduce (fun x y -> x * 10L + y))
|> Seq.iter (printfn "%d")