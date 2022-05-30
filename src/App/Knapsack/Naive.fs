namespace Knapsack

/// <summary>
/// Naive implementation of the 0-1 knapsack problem.
///
/// let cap = 50
/// let values = [| 10; 20; 30 |]
/// let w = [| 60; 100; 120 |]
/// let c = Array.length values
/// Knapsack.Naive.knapsack cap values w c |> printfn "%A"
///
/// </summary>
module Naive =
    let rec knapsack capacity (weights: array<int>) (values: array<int>) counter =
        printfn "Capacity: %A and %A and %A and %A" capacity weights values counter

        if not (counter = 0 || capacity = 0) then
            printfn
                "Is weight %A greater than capacity %A? %A"
                weights[counter - 1]
                capacity
                (weights[counter - 1] > capacity)

        if counter = 0 || capacity = 0 then
            0
        else if weights[counter - 1] > capacity then
            knapsack capacity weights values (counter - 1)
        else
            printfn "Computing..."
            let leftCapacity = capacity - weights[counter - 1]
            printfn "Left capacity %A" leftCapacity

            let newValueIncluded =
                values[counter - 1]
                + knapsack leftCapacity weights values (counter - 1)

            printfn "New value included %A" newValueIncluded
            let withoutNewValue = knapsack capacity weights values (counter - 1)
            printfn "Without new value %A" withoutNewValue
            printfn "%A and %A and %A" newValueIncluded withoutNewValue leftCapacity
            System.Math.Max(newValueIncluded, withoutNewValue)
