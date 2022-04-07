open Origami

let range start stop step =
  let rec f a i =
    let x = start +. i *. step in
    if x >= stop then a
    else f (x :: a) (i +. 1.)
  in 
  f [] 0.

let plot f filename =
  let file = open_out filename in
  let print = Printf.fprintf file "%s" in
  let axis = range (-5.) 11. 0.05 in
  print "x,y,v\n";
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          print
            (string_of_float x ^ "," ^ string_of_float y ^ ","
            ^ string_of_int (f (x, y))
            ^ "\n"))
        axis)
    axis;
    close_out file

(* let a = kolko (3., 3.) 7.;;

plot a "f1.csv"

let a = zloz (5., -10.) (5., 100.) a;;

plot a "f2.csv"

let a = zloz (5., 0.) (5., 0.01) a;;

plot a "f3.csv"

let a = zloz (1., 0.) (1., -1.) a;;

plot a "f4.csv"

let a = zloz (5., 10.) (1., 0.) a;;

plot a "f5.csv"

let a = zloz (1., 0.) (5., 10.) a;;

plot a "f6.csv" *)

let centr = (0., 0.);;

let l = [((5., 0.), (5., 377.)); ((5., 0.), (5., 1.));
	 ((-6., -6.), (-6.1, -6.1)); ((9., 5.), (4., 2.))];;

let a = prostokat centr (10., 10.);;

let a = skladaj l a;;

plot a "g1.csv"