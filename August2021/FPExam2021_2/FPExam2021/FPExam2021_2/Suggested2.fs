module FPExam2021_2.Suggested2

    let rec foo xs ys =
        match xs, ys with
        | [], ys -> ys
        | xs, [] -> xs
        | x :: xs, y :: ys when x < y ->
            x :: (foo xs (y :: ys))
        | x :: xs, y :: ys ->
            y :: (foo (x :: xs) ys)

    and bar =
        function
        | [] -> []
        | [x] -> [x]
        | xs ->
            let (a, b) = List.splitAt (List.length xs / 2) xs
            foo (bar a) (bar b)

(* Question 2.1 *)

    (*

    Q: What are the types of functions foo and bar?

    A: foo has type 'a list -> 'a list -> 'a list when 'a: comparison
       bar has type 'a list -> 'a list when 'a: comparison

    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: Given a list lst, bar returns a sorted list in ascending ordered containing all elements of lst.

    Q: What would be appropriate names for functions
       foo and bar?

    A: foo can be called merge
       bar can be called mergeSort

    Q: What would be appropriate names of the values a and b in bar.


    A: a can be called firstHalf
       b can be called secondHalf

    *)


(* Question 2.2 *)


    (*
    The code includes the keyword "and".


    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: The keyword 'and' is used to declare mutually recursive functions and types.
       Normally, terms are not available before they are declared but we can get around
       this by using the 'and' keyword.

    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: Nothing would happen. The program would still work as `foo` does not call `bar`.

    *)

(* Question 2.3 *)
    let foo2 xs ys =
        List.unfold
            (function
             | [], []                      -> None
             | [], y :: ys                 -> Some (y, ([], ys))
             | x :: xs, []                 -> Some (x, (xs, []))
             | x :: xs, y :: ys when x < y -> Some (x, (xs, y :: ys))
             | x :: xs, y :: ys            -> Some (y, (x::xs, ys)))
            (xs, ys)

    (* use the following code as a starting template
    let foo2 xs ys = List.unfold <a function goes here> (xs, ys)
    *)

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A:
        Im providing the results for both here, but remember you only have to do one (foo is easier)

        foo [1; 3] [2; 4; 5] -->
        1 :: foo [3] [2; 4; 5] -->
        1 :: 2 :: foo [3] [4; 5] -->
        1 :: 2 :: 3 :: foo [] [4; 5] -->
        1 :: 2 :: 3 :: [4; 5] -->
        [1; 2; 3; 4; 5]

        foo is not tail recursive as the elements are appended to a list that depend on the recursive call
        ( 1 :: 2 :: foo [3] [4; 5]) for instance. Until the recursive call is evaluated there is no tail to
        append the elements 1 and 2 to, they get stored ond the stack, which can eventually overflow if the lists
        are big enough.

        bar [8; 7; 6; 5; 4; 3; 2; 1] -->
        foo (bar [8; 7; 6; 5])
            (bar [4; 3; 2; 1]) -->
        foo (foo (bar [8; 7])
                 (bar [6; 5]))
            (foo (bar [4; 3])
                 (bar [2; 1])) -->
        foo (foo (foo (bar [8]) (bar [7]))
                 (foo (bar [6]) (bar [5])))
            (foo (foo (bar [4]) (bar [3]))
                 (foo (bar [2]) (bar [1]))) -->
        foo (foo (foo [8] [7])
                 (foo [6] [5]))
            (foo (foo [4] [3])
                 (foo [2] [1])) -->
        foo (foo [7; 8] [5; 6])
            (foo [3; 4] [1; 2]) -->
        foo [5; 6; 7; 8] [1; 2; 3; 4] -->
        [1; 2; 3; 4; 5; 6; 7; 8]


    You could absolutely get away with a shorter list (length four for instance)

    bar is not tail recursive as it recursively appears in arguments to foo so foo cannot be evaluated until
    bar has returned a result. These nested calls to foo grow the stack and risk a stack overflow.
    *)
(* Question 2.5 *)




    let fooTail xs ys =
        let rec aux cont xs ys =
            match xs, ys with
            | [], ys -> cont ys
            | xs, [] -> cont xs
            | x :: xs, y :: ys when x < y ->
                aux (fun result -> cont (x :: result)) xs (y :: ys)
            | x :: xs, y :: ys ->
                aux (fun result -> cont (y :: result)) (x :: xs) ys

        aux id xs ys
