(* warmups.ml
 * http://courses.cms.caltech.edu/cs11/material/ocaml/lab1/lab1.html
 *)

(* Question 1 *)
let average x y = (x +. y) /. 2.0

let sqrt tol x =
  if x < 0.0
  then invalid_arg "x must be positive"
  else let rec next y1 y2 =
    if abs_float (y1 -. y2) < tol
    then y1
    else next y2 (average y2 (x /. y2))
  in
    next x 1.0

(* Question 2 *)
let sqrt2 = sqrt 0.00001
              
(* Question 3 *)
let rec factorial1 n =
  if n = 0
  then 1
  else if n > 0
  then n * (factorial1 (n - 1))
  else invalid_arg "n must be non-negative"

(* Question 4 *)
let rec factorial2 = function
  | 0 -> 1
  | n when n > 0 -> n * (factorial2 (n - 1))
  | _ -> invalid_arg "n must be non-negative"

(* Question 5 *)
let factorial = function
  | 0 -> 1
  | 1 -> 1
  | n when n > 1 ->
      let rec f = function
        | (acc, k) when k = n -> k * acc
        | (acc, k) -> f((acc * k),  (k + 1))
      in
        f (1, 1)
  | _ -> invalid_arg "n must be non-negative"

(* Question 6 *)
let rec fibonacci = function
  | 0 -> 0
  | 1 -> 1
  | n when n > 1 -> (fibonacci (n - 1)) + (fibonacci (n - 2))
  | _ -> invalid_arg "n must be non-negative"

(* Question 7 *)
let rev lst =
  let rec r = function
    | (acc, []) -> acc
    | (acc, h :: t) -> r (h :: acc, t)
  in
    r ([], lst)

(* Question 8 *)
let rec map f xs =
  match xs with
    | [] -> []
    | (h :: t) -> (f h) :: (map f t)

(* Question 9 *)
let map2 f xs = 
  let rec m = function
    | (acc, []) -> rev acc
    | (acc, h :: t) -> m ((f h) :: acc, t)
  in
    m ([], xs)

(* Question 10 *)
let rec range a b = if (a > b) then [] else a :: range (a + 1) b

(* Question 11 *)
let roots = map (fun x -> sqrt 1e-17 (float_of_int x)) (range 1 20)
