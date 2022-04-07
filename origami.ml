(*       Autor: Jonasz Aleszkiewicz     *)
(* Code Review: Jakub Kołaczyński, gr.5 *)

type point = float * float
type kartka = point -> int

let ( <=.. ) (a, b) (c, d) = a <= c && b <= d
let prostokat p q v = if p <=.. v && v <=.. q then 1 else 0

(* Używamy wewnętrznie liczb zespolonych, bo są niemalże wektorami. *)
open Complex

let complex_of (a, b) = { re = a; im = b }
let point_of p : point = p.re, p.im
let ( +.. ), ( -.. ), ( *.. ), ( /.. ) = add, sub, mul, div
let cross_product a b = (a.re *. b.im) -. (a.im *. b.re)

(** Bardzo mała liczba. *)
let eps = 1e-11

type side = Left | Right | Neither

(** Po której stronie prostej wyznaczonej przez [p, q] leży punkt [v]. *)
let relate (p, q) v =
  match cross_product (q -.. p) (v -.. p) with
  | x when abs_float x < eps -> Neither
  | x when x < 0. -> Right
  | _ -> Left

(** Wektor symetryczny do wektora [v] względem wektora [p]. *)
let reflect_free p v = conj (v /.. p) *.. p

(** Punkt symetryczny do [v] względem prostej wyznaczonej przez [p, q]. *)
let reflect (p, q) v = p +.. reflect_free (q -.. p) (v -.. p)

let kolko o r v =
  let o = complex_of o and v = complex_of v in
  (* Zwiększamy troszeczkę średnicę żeby po straceniu odrobinki precyzji
     punkciki na krawędzi nadal należały do kółeczka. *)
  if norm (o -.. v) <= r +. eps then 1 else 0

let zloz p q f v =
  let pq = complex_of p, complex_of q and v = complex_of v in
  match relate pq v with
  | Left ->
      let v' = reflect pq v in
      f (point_of v) + f (point_of v')
  | Right -> 0
  | Neither -> f (point_of v)

let skladaj l k = List.fold_left (fun k (p, q) -> zloz p q k) k l
