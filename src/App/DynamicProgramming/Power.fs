namespace DynamicProgramming

module Power =

    let rec power x y =
        printfn $"{x} and {y}"

        match y with
        | 0 -> 1.0
        | 1 -> float x
        | b when b < 0 -> 1.0 / power x (-b)
        | _ ->
            let p = power x (y / 2)
            p * p * power x (y % 2)

    let rec powerIfElse a n =
        printfn $"{a} and {n}"

        if n = 0 then
            1.0
        elif n = 1 then
            float a
        elif n < 0 then
            1.0 / powerIfElse a (-n)
        elif n % 2 = 0 then
            powerIfElse (a * a) (n / 2)
        else
            a * powerIfElse (a * a) (n / 2)

    printfn $"{powerIfElse 4 6}"
    printfn $"{powerIfElse -2 -3}"
    power 4 6 |> printfn "%A"
    power 2 3 |> printfn "%A"
    power -2 -3 |> printfn "%A"

    let rec nthPower x n =
        match n with
        | 0 -> 1.0
        | 1 -> double x
        | n when n < 0 -> 1.0 / (nthPower x -n)
        | _ ->
            let p = nthPower x (n / 2)
            p * p * nthPower x (n % 2)
