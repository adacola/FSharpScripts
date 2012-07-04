// タンピン二盃口が成立しているかを判定する。
// 鳴き、カン、七対子形、国士形はタンピン二盃口と両立しないので最初から考慮しない。
// ツモピンフあり、大車輪ありとする。

open System.Text.RegularExpressions
open System.Collections.Generic

let input() =
    printfn "Manzu:1m-9m Pinzu:1p-9p Souzu:1s-9s Jihai:1j-7j (The last input means 'Agari Hai')"
    let inp = System.Console.ReadLine()
    match Regex.Match(inp, @"\A\s*(?:([1-9][mps]|[1-7]j)\s*){14}\Z") with
    | m when m.Success ->
        let hais =
            m.Groups.[1].Captures |> Seq.cast<Capture>
            |> Seq.map (fun c -> c.Value.[1], int c.Value.[.. 0]) |> Seq.toList
        let agariHai = hais |> List.rev |> List.head
        let haiCountMap = hais |> Seq.countBy id |> Map.ofSeq
        if haiCountMap |> Map.forall (fun _ -> (>=) 4) then Some(haiCountMap, agariHai) else None
    | _ -> None

let getAllMentsu haiCountMap agariHai =
    let tryRemoveCount n key map =
        map |> Map.tryFind key |> Option.bind (function
            | c when c > n -> map |> Map.add key (c - n) |> Some
            | c when c = n -> map |> Map.remove key |> Some
            | _ -> None)

    let tryTakeSame count haiCountMap (hai : char * int) =
        tryRemoveCount count hai haiCountMap |> Option.map (fun m -> Map.ofList [hai, count], m)

    let tryTakeShuntsu haiCountMap (suit, num) =
        if suit = 'j' then None else
        (Some haiCountMap, [num .. num + 2])
        ||> List.fold (fun m n -> m |> Option.bind (tryRemoveCount 1 (suit, n)))
        |> Option.map (fun m -> Map.ofList [for n in num .. num + 2 -> (suit, n), 1], m)

    let memo = HashSet(HashIdentity.Structural)
    let rec getMentsu haiCountMap mentsus =
        if memo.Add(haiCountMap, List.sort mentsus) |> not then Seq.empty
        elif Map.isEmpty haiCountMap then
            let mentsus = mentsus |> List.rev
            mentsus |> Seq.mapi (fun i m -> i, m) |> Seq.choose (fun (i, mentsu) ->
                tryRemoveCount 1 agariHai mentsu |> Option.map (fun m ->
                    mentsus |> List.mapi (fun j mentsu -> if j = i then m else mentsu)))
            |> Seq.map (fun (jantou::rest) -> jantou::List.sortBy Seq.length rest)
        elif List.isEmpty mentsus then
            haiCountMap |> Map.toSeq |> Seq.choose (fst >> tryTakeSame 2 haiCountMap)
            |> Seq.collect (fun (jantou, rest) -> getMentsu rest [jantou])
        else
            haiCountMap |> Map.toSeq |> Seq.collect (fun (hai, _) ->
                [tryTakeSame 3; tryTakeShuntsu] |> Seq.choose (fun f-> f haiCountMap hai)
                |> Seq.collect (fun (mentsu, rest) -> getMentsu rest (mentsu::mentsus)))

    getMentsu haiCountMap []

let isYaochuu = function 'j', _ | _, 1 | _, 9 -> false | _ -> true

let isTanyao agariHai mentsus =
    mentsus |> List.forall (Map.toSeq >> Seq.forall (fst >> isYaochuu)) && isYaochuu agariHai

let isPinfu agariHai (jantou::taatsu::rest) =
    let isSequence count mentsu =
        let hais = mentsu |> Map.toSeq |> Seq.toList
        List.length hais = count
        && hais |> Seq.map (fst >> snd) |> Seq.sort |> Seq.pairwise
            |> Seq.map ((<||) (-)) |> Seq.forall ((=) -1)

    jantou |> Map.forall (fun _ -> (=) 2) && taatsu |> isSequence 2
    && rest |> List.forall (isSequence 3)

let isRyanpeikou agariHai (jantou::taatsu::rest) =
    let mentsus =
        if jantou |> Map.forall (fun _ -> (=) 1) then taatsu::rest else
        let c = Map.tryFind agariHai taatsu in Map.add agariHai (defaultArg c 0 + 1) taatsu::rest
    let counts = mentsus |> Seq.countBy id |> Seq.toList
    (List.length counts = 2 && counts |> List.forall (snd >> (=) 2))
    || (List.length counts = 1 && counts |> List.forall (snd >> (=) 4))

let isDaisharin agariHai =
    List.collect (Map.toList >> List.collect (fun (hai, count) -> List.replicate count hai))
    >> (fun hais -> agariHai::hais) >> Seq.countBy id >> Seq.toList >> List.unzip
    >> fun (hais, counts) ->
        let suits, nums = List.unzip hais
        suits |> Seq.distinct |> Seq.length |> (=) 1 && Seq.head suits <> 'j'
        && nums |> List.forall (fun num -> 1 < num && num < 9)
        && counts |> List.forall ((=) 2)

let main() =
    match input() with
    | Some(haiCountMap, agariHai) ->
        getAllMentsu haiCountMap agariHai |> Seq.exists (fun mentsus ->
            [isTanyao agariHai; isPinfu agariHai; isRyanpeikou agariHai; isDaisharin agariHai >> not]
            |> List.forall ((|>) mentsus))
        |> printfn "%O"
    | _ -> printfn "Illegal input"

main()