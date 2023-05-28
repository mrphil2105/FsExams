module June2021.Suggested1
(* 1: Dungeon crawler *)

(* Question 1.1 *)

    type direction = North | East | South | West
    type coord     = C of int * int

    let move dist dir (C (x, y)) =
        match dir with
        | North -> C (x, y - dist)
        | East -> C (x + dist, y)
        | South -> C (x, y + dist)
        | West -> C (x - dist, y)

    let turnRight =
        function
        | North -> East
        | East -> South
        | South -> West
        | West -> North

    let turnLeft =
        function
        | North -> West
        | West -> South
        | South -> East
        | East -> North

(* Question 1.2 *)

    type position = P of (coord * direction)
    type move     = TurnLeft | TurnRight | Forward of int

    let step (P (c, dir)) =
        function
        | TurnLeft -> P (c, turnLeft dir)
        | TurnRight -> P (c, turnRight dir)
        | Forward dist -> P (move dist dir c, dir)

(* Question 1.3 *)


    let rec walk p =
        function
        | [] -> p
        | x :: xs -> walk (step p x) xs

    let rec walk2 = List.fold step

(* Question 1.4 *)

    let rec path (P (c, _) as p) =
        function
        | [] -> [c]
        | (Forward _ as x) :: xs -> c::(path (step p x) xs)
        | x :: xs                -> path (step p x) xs

(* Question 1.5 *)

    let path2 p ps =
        let rec aux (P (c, _) as p) acc =
            function
            | [] -> List.rev (c::acc)
            | (Forward _ as x) :: xs ->  aux (step p x) (c::acc) xs
            | x :: xs -> aux (step p x) acc xs

        aux p [] ps

(* Question 1.6 *)

(* Q: Your solution for `path` is not tail recursive. Why? To make a compelling
      argument you should evaluate a function call of the function, similarly to
      what is done in Chapter 1.4 of HR, and reason about that evaluation.
      You need to make clear what aspects of the evaluation tell you that the
      function is not tail recursive. Keep in mind that all steps in an evaluation
      chain must evaluate to the same value
      (```(5 + 4) * 3 --> 9 * 3 --> 27```, for instance).

   A:

     Consider the following derivation
     This is much longer than strictly required (two forwards to demonstrate the building list would suffice)
     but it shows a more complete derivation.

     path (P (C (0, 0), North))
          [Forward 5; TurnRight; Forward 5; TurnRight;
           Forward 5; TurnRight; Forward 5] -->

        C (0, 0)::path (step (P (C (0, 0), North)) (Forward 5))
                        [TurnRight; Forward 5; TurnRight; Forward 5; TurnRight; Forward 5] -->

        C (0, 0) :: path (P (C (0, -5), North))
                             [TurnRight; Forward 5; TurnRight; Forward 5; TurnRight; Forward 5] -->

        C (0, 0) :: path (step (P (C (0, -5), North)) TurnRight)
                                [Forward 5; TurnRight; Forward 5; TurnRight; Forward 5] -->

        C (0, 0) :: path (P (C (0, -5), East))
                             [Forward 5; TurnRight; Forward 5; TurnRight; Forward 5] -->

        C (0, 0) :: C (0, -5) :: path (step (P (C (0, -5), East)) (Forward 5))
                                      [TurnRight; Forward 5; TurnRight; Forward 5] -->

        C (0, 0) :: C (0, -5) :: path (P (C (5, -5), East))
                                      [TurnRight; Forward 5; TurnRight; Forward 5] -->

       C (0, 0) :: C (0, -5) :: path (step (P (C (5, -5), East)) TurnRight)
                                     [Forward 5; TurnRight; Forward 5] -->

       C (0, 0) :: C (0, -5) :: path (P (C (5, -5), South))
                                     [Forward 5; TurnRight; Forward 5] -->

       C (0, 0) :: C (0, -5) :: C (5, -5) :: path (step (P (C (5, -5), South)) (Forward 5))
                                                  [TurnRight; Forward 5] -->

       C (0, 0) :: C (0, -5) :: C (5, -5) :: path (P (C (5, 0), South))
                                                  [TurnRight; Forward 5] -->

       C (0, 0) :: C (0, -5) :: C (5, -5) :: path (step (P (C (5, 0), South)) TurnRight)
                                                  [Forward 5] -->

       C (0, 0) :: C (0, -5) :: C (5, -5) :: path (step (P (C (5, 0), West)) (Forward 5)) [] -->

       C (0, 0) :: C (0, -5) :: C (5, -5) :: C (5, 0) :: path (P (C (0, 0), West)) [] -->

       C (0, 0) :: C (0, -5) :: C (5, -5) :: C (5, 0) :: [C (0, 0)] -->

       [C (0, 0); C (0, -5); C (5, -5); C (5, 0); C (0, 0)]

       The reason that this function is not tail recursive is that there are pending computations that
       cannot be resolved until the final recursive call has been made. In particular, there are
       cons-operations of the coordinate list that keep on growing (and are stored on the stack) and
       cannot be resolved into a single list until the base case of the function has been reached.

*)
    let path3 p ps =
        let rec aux (P (c, _) as p) cont =
            function
            | []                     -> cont [c]
            | (Forward _ as x) :: xs -> aux (step p x) (fun res -> cont (c::res)) xs
            | x :: xs                -> aux (step p x) cont xs

        aux p id ps
