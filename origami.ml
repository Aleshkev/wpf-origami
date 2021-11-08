(* Autor: Jonasz Aleszkiewicz *)

type point = float * float
type kartka = point -> int

open Complex

let eps = 1e-11

(* Używamy wewnętrznie liczb zespolonych, bo są takimi wektorami
   zaimplementowanymi w bibliotece standardowej. Upraszczają obliczenia
   geometryczne. *)
let complex_of (a, b) = { re = a; im = b }
let point_of p : point = (p.re, p.im)
let ( +.. ), ( -.. ), ( *.. ), ( /.. ) = (add, sub, mul, div)
let cross_product a b = (a.re *. b.im) -. (a.im *. b.re)

type side = Left | Line | Right

(** Po której stronie prostej wyznaczonej przez [p], [q] leży punkt [v]. *)
let relate p q v =
  match cross_product (q -.. p) (v -.. p) with
  | x when abs_float x < eps -> Line
  | x when x < 0. -> Right
  | _ -> Left

(** Wektor symetryczny do wektora [v] względem wektora [p]. *)
let reflect_free p v = conj (v /.. p) *.. p

(** Punkt symetryczny do [v] względem prostej wyznaczonej przez [p], [q]. *)
let reflect p q v = p +.. reflect_free (q -.. p) (v -.. p)

let ( <=.. ) (a, b) (c, d) = a <= c && b <= d
let prostokat p q v = if p <=.. v && v <=.. q then 1 else 0

let kolko o r v =
  let o = complex_of o and v = complex_of v in
  (* Dodajemy [eps] żeby po straceniu odrobinki precyzji punkciki na krawędzi
     nadal należały do koła. *)
  if norm (o -.. v) <= r +. eps then 1 else 0

let zloz p q f v =
  let p = complex_of p and q = complex_of q and v = complex_of v in
  match relate p q v with
  | Line -> f (point_of v)
  | Right -> 0
  | Left ->
      let v' = reflect p q v in
      f (point_of v) + f (point_of v')

let skladaj l k = List.fold_left (fun k (p, q) -> zloz p q k) k l
