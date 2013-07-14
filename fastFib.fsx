open System.Collections.Generic

let inline memo (m : Dictionary<_, _>) f x =
    match m.TryGetValue x with
    | true, r -> r
    | _ ->
        let r = f x
        m.[x] <- r
        r

let fib =
    let m = Dictionary()
    let inline memo f x = memo m f x

    let rec fib n =
        let fibEven n =
            let half = memo fib (n / 2)
            half * (half + 2I * memo fib (n / 2 - 1))
        let fibOdd n =
            bigint.Pow(memo fib (n / 2 + 1), 2) + bigint.Pow(memo fib (n / 2), 2)
        if n % 2 = 0 then memo fibEven n else memo fibOdd n

    let rec fibBase = function 0 -> 0I | 1 -> 1I | n -> memo fibBase (n - 1) + memo fibBase (n - 2)
    let fibBase = memo fibBase

    [0 .. 10] |> List.iter (fibBase >> ignore)

    function
    | n when n < 0 -> invalidArg "n" "0以上の整数を指定してください"
    | n when n <= 10 -> fibBase n
    | n -> fib n
