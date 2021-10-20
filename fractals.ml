(*2. FRACTALS
Test de la bibliothèque graphique*)

#load "graphics.cma";;
open Graphics;;
open_graph " 1200x800";;

let test (x, y) (z, t) =
  clear_graph ();
  set_color red;
  moveto x y ;
  lineto z t ;
  set_color blue ;
  rmoveto x y ;
  rlineto z t ;


(* 2.1 - Les courbes
2.1.1 - Mountain *)

clear_graph();;
open Random;;

let line (x,y) (z,t) =
    moveto x y;
    lineto z t ;;

let rec montagne x y z t = function
	| 0 -> moveto x y; lineto z t
	| n -> 
		let h = (y+t)/2 + int(abs(z-x)/5 + 20) and u = x+(z-x)/2 in
				montagne x y u h (n-1);
				montagne u h z t (n-1);;

montagne 20 20 500 20 8;;

(*2.1.2 - Dragon*)

let rec dragondefeu n (x,y) (z,t) =
  if n = 1 then
     begin
      moveto x y;
      lineto z t;
     end
  else
      let u = (x + z + t - y) / 2
      and v = (y + t - z + x) / 2
      in begin
          dragondefeu (n-1) (x,y) (u,v);
          dragondefeu (n-1) (z,t) (u,v);
    end ;;


(*2.2 - Les surfaces
2.2.1 Sierpinski carpet*)

let eponge (x,y) n =
  moveto x y;
  set_color black;
  fill_rect x y n n;
  set_color white; 
  let rec sierpinsky (x,y) n = function
    | 0 -> ()
    | p -> let n3 = (n/3) in
      let x1 = x+n3 and x2 = x + 2*n3 and y1 = y + n3 and y2 = y + 2*n3
      in
      fill_rect x1 y1 n3 n3;
      sierpinsky (x,y) n3 (p-1);
      sierpinsky (x1,y) n3 (p-1);
      sierpinsky (x2,y) n3 (p-1);
      sierpinsky (x,y1) n3 (p-1);
      sierpinsky (x2,y1) n3 (p-1);
      sierpinsky (x,y2) n3 (p-1);
      sierpinsky (x1,y2) n3 (p-1);
      sierpinsky (x2,y2) n3 (p-1)
  in sierpinsky (x,y) n 5;;

clear_graph();;
eponge (100,100) 500;;

(*2.2.2 - Sierpinski triangle*)

#load "graphics.cma" ;;
open Graphics ;;
open_graph "" ;;

let draw_triangle (x1, y1) (x2, y2) (x3, y3) =
  moveto x1 y1;
  lineto x2 y2;
  lineto x3 y3;
  lineto x1 y1 ;;

let middle (x1, y1) (x2, y2) = 
  ((x1 + x2) / 2,(y1 + y2) / 2);;
   
let rec sierpinski_rec level p1 p2 p3 =
  if 0 = level then
    draw_triangle p1 p2 p3
  else (
    sierpinski_rec (level - 1) p1 (middle p1 p2) (middle p1 p3);
    sierpinski_rec (level - 1) p2 (middle p2 p1) (middle p2 p3);
    sierpinski_rec (level - 1) p3 (middle p3 p1) (middle p3 p2);
   );;

let sierpinski level = 
  clear_graph ();
  sierpinski_rec level (5, 5) (505, 505) (1005, 5);;

sierpinski 10;;

(* 2.3.3 - Koch snowflake *)

let rec koch (x1,y1) (x5,y5) = function
  | 0 -> (moveto x1 y1; lineto x5 y5)
  | n -> 
      let x2 = x1 + (x5 - x1) / 3 and y2 = y1 + (y5 - y1) / 3 in
      let x4 = x1 + 2 * (x5 - x1) / 3 and y4 = y1 + 2 * (y5 - y1) / 3 in
      let x3 = (x2 + x4)/2 - int_of_float(float_of_int(y4 - y2)*.(sqrt(3.)/.2.))
      and y3 = (y2 + y4)/2 + int_of_float(float_of_int(x4 - x2)*.(sqrt(3.)/.2.)) in
      koch (x1,y1) (x2,y2) (n-1);
      koch (x2,y2) (x3,y3) (n-1);
      koch (x3,y3) (x4,y4) (n-1);
      koch (x4,y4) (x5,y5) (n-1);;

clear_graph();;
koch (100,100) (500,500) 5;;
koch (0,0) (500,500) 5;;

(* 2.3.4 - Vicsek *)

#load "graphics.cma" ;;
open Graphics ;;
open_graph "" ;;


let draw_square (x, y) size =
  fill_rect x y size size;;

let translate (x, y) (vx, vy) = (x + vx, y + vy)

let rec vicsek_etoile_rec level pos size =
  if 0 = level then
    draw_square pos size
  else
    let level = level - 1 and size = size / 3 in (
    vicsek_etoile_rec level pos size;
    vicsek_etoile_rec level (translate pos (0, size * 2)) size;
    vicsek_etoile_rec level (translate pos (size, size)) size;
    vicsek_etoile_rec level (translate pos (size * 2, 0)) size;
    vicsek_etoile_rec level (translate pos (size * 2, size * 2)) size 
   ) ;;

let rec vicsek_croix_rec level pos size =
  if 0 = level then
    draw_square pos size
  else
    let level = level - 1 and size = size / 3 in (
    vicsek_croix_rec level (translate pos (0, size)) size;
    vicsek_croix_rec level (translate pos (size, size * 2)) size;
    vicsek_croix_rec level (translate pos (size, size)) size;
    vicsek_croix_rec level (translate pos (size, 0)) size;
    vicsek_croix_rec level (translate pos (size * 2, size)) size 
   ) ;;

let vicsek_etoile level = 
  clear_graph ();
  vicsek_etoile_rec level (5, 5) 500;;

let vicsek_croix level = 
  clear_graph ();
  vicsek_croix_rec level (5, 5) 500;;

vicsek_croix 5;;
vicsek_etoile 5;;

(* 2.3.5 - Mandelbrot *)
