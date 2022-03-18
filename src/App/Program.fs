let input = [| 1; 2; 3; 4; 5; 4; 3; 2; 1 |]

DynamicProgramming.Peak.peak input |> printfn "%A"

DynamicProgramming.Power.power -2 -3
|> printfn "%A"
