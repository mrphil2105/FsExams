module June2021.Question3

open System

// 3.1

type element = char list

// 3.2

let elToString (el: element) =
    String.Concat el

let elFromString (str: string): element =
    Seq.toList str

// 3.3

let nextElement (el: element): element =
    let rec aux acc cur count =
        function
        | [] -> cur :: char (count + int '0') :: acc
        | x :: xs ->
            if x = cur then aux acc cur (count + 1) xs
            else aux (cur :: char (count + int '0') :: acc) x 1 xs
    aux [] el[0] 0 el |> List.rev

    (*let rec aux acc cur pos count =
        if pos = List.length el then acc//cur :: char (count + int '0') :: acc
        else
            let value = el[pos]
            if value = cur then aux acc cur (pos + 1) (count + 1)
            else aux (cur :: char (count + int '0') :: acc) value (pos + 1) 0
    aux [] el[0] 0 0 |> List.rev*)

(*
// 3.1

type element = (int * int) list

// 3.2

let elToString (el: element) =
    List.fold (fun acc (count, num) ->
        string num :: string count :: acc
    ) [] el
    |>  List.rev
    |>  String.Concat

let elFromString (str: string): element =
    Seq.toList str
    |>  List.chunkBySize 2
    |>  List.map (function
                    | [a; b] -> (a, b)
                    | _ -> failwith "Invalid input.")
    |>  List.fold (fun acc (count, num) ->
            (int (count - '0'), int (num - '0')) :: acc
        ) []
    |> List.rev
*)
