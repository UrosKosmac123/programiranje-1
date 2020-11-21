let rec je_sodo = function
  | 0 -> true
  | n -> je_liho (n - 1)
and je_liho = function
  | 0 -> false
  | n -> je_sodo (n - 1)

let rec multiply sez = match sez with
| e::rest -> e * (multiply rest)
| [] -> 1

let skalarni_produkt (x1, x2, x3) (y1, y2, y3) = 
    x1 *. y1 +. x2 *. y2 +. x3 *. y3

let rec dolzina_seznama sez = 
  match sez with
  | [] -> 0
  | glava :: rep -> 1 + dolzina_seznama rep

let rec preslikaj sez =
  match sez with
  | [] -> []
  | glava::rep -> (glava + 1)::preslikaj rep