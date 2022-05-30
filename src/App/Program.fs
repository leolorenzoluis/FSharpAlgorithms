// printfn "Algorithms..."
// let input = [| 1; 1; 2; 2; 5; 4 |]

// DivideAndConquer.Peak.peak input |> printfn "%A"

// DynamicProgramming.Power.nthPower -2 -3
// |> printfn "%A"



// CellularAutomata.EntryPoint.start [| "30"
//                                      "50" |]
// |> ignore

let cap = 20
let values = [| 40; 20; 30 |]
let w = [| 60; 100; 120 |]
let c = Array.length values

Knapsack.Naive.knapsack cap values w c
|> printfn "%A"


let rec fib n direction =
    printfn " %s  : n = %A" direction n

    if n <= 1 then
        n
    else
        let a = fib (n - 1) "LEFT" + fib (n - 2) "RIGHT"
        printfn " RESULT: %A n = %A" a n
        a

let memoize f = 
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    printfn "Memoize %A" cache
    (fun x ->
        let succ, v = cache.TryGetValue x
        if succ then 
            printfn "Cache hit for %A" x
            v
        else
            let v = f x
            (x, v) |> cache.Add
            v
        )
#nowarn "40"
let rec memoizedFib = 
    printfn "WTF"
    memoize (fun n ->
      if n < 1 then 1 else
      (memoizedFib (n - 1)) + (memoizedFib (n - 2)))
#time
memoizedFib 100
#time
fib 30 "ROOT"

)

for i in [ 1..10 ] do
    fib i |> printfn "%A"
