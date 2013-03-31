(*
【お題】
[1, 2, 3, 4, 5, 6, 7]のようなリストを
次のような表に整形して出力せよ

<table>
<tr><td>1</td><td>2</td><td>3</td></tr>
<tr><td>4</td><td>5</td><td>6</td></tr>
<tr><td>7</td><td>&nbsp;</td><td>&nbsp;</td></tr>
</table>
*)

open System.Text

let makeTable columnCount xs =
    let addHeader (sb : StringBuilder) = sb.AppendLine "<table>"
    let addFooter (sb : StringBuilder) = sb.AppendLine "</table>"
    let addRow sb xs =
        let addRowStart (sb : StringBuilder) = sb.Append "<tr>"
        let addRowEnd (sb : StringBuilder) = sb.AppendLine "</tr>"
        let xs = xs |> Seq.map (function Some x -> x |> box |> string | None -> "&nbsp;")
        (sb |> addRowStart, xs) ||> Seq.fold (fun sb column -> sb.Append("<td>").Append(column).Append("</td>"))
        |> addRowEnd
    let split count xs =
        let rec split xs = seq {
            if xs |> Seq.head |> Option.isSome then
                yield xs |> Seq.take count
                yield! split (Seq.skip count xs) }
        xs |> Seq.map Some |> Seq.cache |> Seq.append <| Seq.initInfinite (fun _ -> None)
        |> split
    let rows = xs |> split columnCount
    let sb = (StringBuilder() |> addHeader, rows) ||> Seq.fold addRow |> addFooter
    sb.ToString()
