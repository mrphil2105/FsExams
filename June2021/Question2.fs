module June2021.Question2

let foo f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y when Map.containsKey x m -> y
        | None ->
            m <- Map.add x (f x) m; f x
    aux
let rec bar x =
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)
and baz = foo bar

// 2.3

let foo2 f =
    let mutable m = Map.empty
    let aux x =
        match Map.tryFind x m with
        | Some y -> y
        | None ->
            let y = f x
            m <- Map.add x y m; y
    aux

// 2.4

let rec barbaz x =
    let baz = foo barbaz
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

// 2.5

let bazSeq =
    Seq.initInfinite id
    |> Seq.map baz
    |> Seq.cache
