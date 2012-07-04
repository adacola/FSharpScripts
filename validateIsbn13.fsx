﻿let modulous10weight31 xs =
    Seq.initInfinite (fun _ -> [1; 3]) |> Seq.concat |> Seq.map2 (*) xs
    |> Seq.sum |> fun s -> (10 - (s % 10)) % 10

let validateIsbn13 isbn =
    String.length isbn = 13
    && List.exists ((=) isbn.[.. 2]) ["978"; "979"]
    && isbn.[.. 11] |> Seq.map (string >> int) |> modulous10weight31 |> (=) (int isbn.[12 .. 12])

validateIsbn13 "9784101092058" |> (=) true |> printfn "%A"
validateIsbn13 "9784062772211" |> (=) true |> printfn "%A"
validateIsbn13 "9784150315684" |> (=) false |> printfn "%A"
