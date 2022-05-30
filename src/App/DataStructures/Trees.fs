module App.DataStructures.Trees

type Tree<'a> =
    | Empty
    | Node of value: 'a * left: Tree<'a> * right: Tree<'a>

let isBST tree =
    let rec verify lo hi tree =
        match tree with
        | Empty -> true
        | Node(value, left, right) ->
            match lo, hi with
            | Some lo, _ when value < lo -> false
            | _, Some hi when value > hi -> false
            | _ ->
                let hi = defaultArg hi value |> min value |> Some
                let lo = defaultArg lo value |> max value |> Some
                verify lo hi left && verify lo hi right
    verify None None tree


let tree2 =
  Node (7,
    Node (3, Empty, Empty),
    Node (9, Empty, Empty))
