module FPExam2021_2.Suggested1

    type binList<'a, 'b> =
        | Nil
        | Cons1 of 'a * binList<'a, 'b>
        | Cons2 of 'b * binList<'a, 'b>

    let rec length =
        function
        | Nil -> 0
        | Cons1 (_, xs) -> 1 + length xs
        | Cons2 (_, xs)  -> 1 + length xs

    let lengthAcc lst =
        let rec aux acc =
            function
            | Nil -> acc
            | Cons1 (_, xs) -> aux (acc + 1) xs
            | Cons2 (_, xs) -> aux (acc + 1) xs

        aux 0 lst

    let length2 lst =
        let rec aux ((l1, l2) as acc) =
            function
            | Nil -> acc
            | Cons1 (_, xs) -> aux (l1 + 1, l2) xs
            | Cons2 (_, xs) -> aux (l1, 1 + l2) xs

        aux (0, 0) lst

    let rec split =
        function
        | Nil -> ([], [])
        | Cons1 (x, xs) ->
            let (r1, r2) = split xs
            (x::r1, r2)
        | Cons2 (y, ys) ->
            let (r1, r2) = split ys
            (r1, y :: r2)

    let rec map f g =
        function
        | Nil -> Nil
        | Cons1(x, xs) -> Cons1 (f x, map f g xs)
        | Cons2(x, xs) -> Cons2 (g x, map f g xs)

    let rec filter f g =
        function
        | Nil -> Nil
        | Cons1 (x, xs) when f x -> Cons1(x, filter f g xs)
        | Cons1 (x, xs)          -> filter f g xs
        | Cons2 (x, xs) when g x -> Cons2(x, filter f g xs)
        | Cons2 (x, xs)          -> filter f g xs

    let rec fold f g acc =
        function
        | Nil -> acc
        | Cons1 (x, xs) -> fold f g (f acc x) xs
        | Cons2 (x, xs) -> fold f g (g acc x) xs
