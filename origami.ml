(* Autor: Jonasz Aleszkiewicz *)

type point = float * float
type kartka = point -> int

open Complex

let debug s t =
  print_endline @@ (Printf.sprintf "%10s" s) ^ "  "
  ^ String.concat "  "
      (List.map (fun x -> Printf.sprintf "%7.2f %7.2f" x.re x.im) t)

let as_complex (a, b) = { re = a; im = b }
let as_point p : point = (p.re, p.im)
let ( +.. ), ( -.. ), ( *.. ), ( /.. ) = (add, sub, mul, div)
let cross_product a b = (a.re *. b.im) -. (a.im *. b.re)

let reflect p v =
  let r = conj (v /.. p) *.. p in
  debug "reflect" [ p; v; r ];
  r

let prostokat p q v =
  let ( <=.. ) (a, b) (c, d) = a <= c && b <= d in
  debug "prostokat" [ as_complex p; as_complex q; as_complex v ];
  if p <=.. v && v <=.. q then 1 else 0

let kolko o r v =
  let o = as_complex o and v = as_complex v in
  if norm2 (o -.. v) <= r ** 2. then 1 else 0

let zloz p q f v =
  let q = as_complex q and p = as_complex p and v = as_complex v in
  debug "zloz" [ p; q; v ];
  match cross_product (q -.. p) (v -.. p) with
  | x when x < 0. -> 0
  | x when abs_float x < 1e-11 -> f (as_point v)
  | _ ->
      let v' = p +.. reflect (q -.. p) (v -.. p) in
      debug "zloz v'" [ p; q; v; v' ];
      f (as_point v) + f (as_point v')

let skladaj l k = List.fold_left (fun k (p, q) -> zloz p q k) k l
