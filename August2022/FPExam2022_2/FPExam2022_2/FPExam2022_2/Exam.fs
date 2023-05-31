module Exam2022_2

    open System

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but
   it does allow you to work in interactive mode and you can just remove the '='
   to make the project compile again.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022_2 =
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale

    let img =
      Quad (Square 255uy,
            Square 128uy,
            Quad(Square 255uy,
                 Square 128uy,
                 Square 192uy,
                 Square 64uy),
            Square 0uy)

(* Question 1.1 *)
    let maxDepth _ = failwith "not implemented"

(* Question 1.2 *)
    let mirror _ = failwith "not implemented"

(* Question 1.3 *)
    let operate _ = failwith "not implemented"

    let mirror2 _ = failwith "not implemented"

(* Question 1.4 *)

    let compress _ = failwith "not implemented"

(* 2: Code Comprehension *)
    let rec foo f =
        function
        | []               -> []
        | x :: xs when f x -> x :: (foo f xs)
        | _ :: xs          -> foo f xs

    let rec bar fs xs =
        match fs with
        | []       -> xs
        | f :: fs' -> bar fs' (foo f xs)

(* Question 2.1 *)

    (*

    Q: What are the types of functions foo and bar?

    A: <Your answer goes here>


    Q: What do the functions foo and  bar do.
       Focus on what it does rather than how it does it.

    A: <Your answer goes here>

    Q: What would be appropriate names for functions
       foo and bar?

    A: <Your answer goes here>

    Q: The function foo uses an underscore `_` in its third case.
       Is this good coding practice, if so why, and if not why not?

    A: <Your answer goes here>
    *)


(* Question 2.2 *)

    let bar2 fs xs =
        List.fold (fun xs f ->
            List.filter f xs
        ) xs fs

(* Question 2.3 *)

    let baz fs =
        fun x ->
            let rec aux =
                function
                | [] -> true
                | f :: fs when f x -> aux fs
                | _ -> false
            aux fs

(* Question 2.4 *)

    (*

    Q: Only one of the functions `foo` and `bar` is tail recursive. Which one?
       Demonstrate why the other one is not tail recursive.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

       let rec foo f =
            function
            | []               -> []
            | x :: xs when f x -> x :: (foo f xs)
            | _ :: xs          -> foo f xs

    A: Foo is not tail recursive, since 'x :: (foo f xs)' depends on the return value of the foo call.

       Function evaluation:

       --- goes here ---

    *)
(* Question 2.5 *)
    // only implement the one that is NOT already tail recursive
    let fooTail f xs =
        let rec aux xs c =
            match xs with
            | [] -> c []
            | x :: xs when f x -> aux xs (fun r -> c (x :: r))
            | _ :: xs -> aux xs c
        aux xs id

    let barTail _ = failwith "not implemented"


(* 3: Guess a number *)

    type guessResult = Lower | Higher | Equal
    type oracle =
        { max : int
          f : int -> guessResult }

(* Question 3.1 *)

    let getTarget o =
        let rec aux target x =
            match o.f x with
            | Equal when Option.isNone target -> aux (Some x) (x + 1)
            | Equal when Option.isSome target -> None
            | _ when x < o.max -> aux target (x + 1)
            | _ -> target
        aux None 1

    let validOracle o =
        let target = getTarget o
        match target with
        | None -> false
        | Some target ->
            let rec aux target x =
                match o.f x with
                | _ when x > o.max -> true
                | Lower when x > target -> aux target (x + 1)
                | Higher when x < target -> aux target (x + 1)
                | Equal when x = target -> aux target (x + 1)
                | _ -> false
            aux target 1


(* Question 3.2 *)

    let randomOracle m oseed =
        let random = if Option.isSome oseed then Random(Option.get oseed) else Random()
        let r = random.Next(1, m + 1)
        { max = m; f =
            fun x ->
                match x with
                | _ when x < r -> Higher
                | _ when x > r -> Lower
                | _  -> Equal }

(* Question 3.3 *)

    let findNumber o =
        let rec aux a b gs =
            let g = a + ((b - a) / 2)
            match o.f g with
            | Lower -> aux a (g - 1) (g :: gs)
            | Higher -> aux (g + 1) b (g :: gs)
            | Equal -> gs
        aux 1 o.max []

(* Question 3.4 *)
    let evilOracle _ = failwith "not implemented"

(* Question 3.5 *)
    let parFindNumber os =
        List.map
            (fun o ->
                async {
                    return findNumber o
                }
            ) os
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.toList

(* 4: Assembly *)

    type register = R1 | R2 | R3
    type address = uint

    type assembly =
    | MOVI of register * int
    | MULT of register * register * register
    | SUB of register * register * register
    | JGTZ of register * address


    let factorial x =           // Address
        [MOVI (R1, 1)           // 0
         MOVI (R2, x)           // 1
         MOVI (R3, 1)           // 2
         MULT (R1, R1, R2)      // 3 (Loop starts here)
         SUB  (R2, R2, R3)      // 4
         JGTZ (R2, 3u)]         // 5 (Loop ends here)

(* Question 4.1 *)

    type program = unit (* replace this entire type with your own *)
    let assemblyToProgram _ = failwith "not implemented"

(* Question 4.2 *)

    type state = unit (* replace this entire type with your own *)
    let emptyState _ = failwith "not implemented"


(* Question 4.3 *)

    let setRegister _ = failwith "not implemented"

    let getRegister _ = failwith "not implemented"

    let setProgramCounter _ = failwith "not implemented"

    let getProgramCounter _ = failwith "not implemented"

    let getProgram _ = failwith "not implemented"

(* Question 4.4 *)

    type StateMonad<'a> = SM of (state -> 'a * state)

    let ret x = SM (fun s -> x, s)
    let bind f (SM a) : StateMonad<'b> =
      SM (fun s ->
      let x, s' = a s
      let (SM g) = f x
      g s')

    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM prog (SM f) = f (emptyState prog)

    let setReg _ = failwith "not implemented"

    let getReg _ = failwith "not implemented"

    let setPC _ = failwith "not implemented"

    let incPC _ = failwith "not implemented"

    let lookupCmd _ = failwith "not implemented"



(* Question 4.5 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let runProgram _ = failwith "not implemented"
