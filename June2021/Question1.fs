module June2021.Question1

// 1.1

type direction = North | East | South | West
type coord = C of int * int

let move dist dir (C (x, y)) =
    match dir with
    | North -> C (x, y - dist)
    | South -> C (x, y + dist)
    | West -> C (x - dist, y)
    | East -> C (x + dist, y)

let turnRight dir =
    match dir with
    | North -> East
    | South -> West
    | West -> North
    | East -> South

let turnLeft dir =
    match dir with
    | North -> West
    | South -> East
    | West -> South
    | East -> North

// 1.2

type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

let step (P (coord, dir)) mov =
    match mov with
    | TurnRight -> P (coord, turnRight dir)
    | TurnLeft -> P (coord, turnLeft dir)
    | Forward dist -> P (move dist dir coord, dir)

// 1.3

let rec walk pos moves =
    match moves with
    | [] -> pos
    | move :: xs -> walk (step pos move) xs

let walk2 pos moves =
    List.fold (fun acc move ->
        step acc move
    ) pos moves

// 1.4

let rec path pos moves =
    match moves with
    | [] ->
        let (P (coord, _)) = pos
        [coord]
    | move :: xs ->
        match move with
        | Forward _ ->
            let (P (coord, _)) = pos
            coord :: path (step pos move) xs
        | _ -> path (step pos move) xs

// 1.5

let path2 (P (coord, dir)) moves =
    let rec aux (acc: coord list) pos moves =
        match moves with
        | [] -> List.rev acc
        | move :: xs ->
            match move with
            | Forward _ ->
                let (P (coord, dir)) = step pos move
                aux (coord :: acc) (P (coord, dir)) xs
            | _ -> aux acc (step pos move) xs
    aux [coord] (P (coord, dir)) moves

// 1.6

let path3 (P (coord, dir)) moves =
    let rec aux pos moves c =
        match moves with
        | [] ->
            let (P (coord, _)) = pos
            c [coord]
        | move :: xs ->
            match move with
            | Forward _ ->
                let (P (coord, _)) = pos
                aux (step pos move) xs (fun r -> c (coord :: r))
            | _ -> aux (step pos move) xs c
    aux (P (coord, dir)) moves id
