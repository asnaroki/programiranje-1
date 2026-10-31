(*----------------------------------------------------------------------------*
 # Uvod v OCaml
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Vektorji
*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razteg : float -> float list -> float list`, ki vektor,
 predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
*----------------------------------------------------------------------------*)

let razteg f v = List.map (fun x -> f *. x) v

let primer_vektorji_1 = razteg 2.0 [1.0; 2.0; 3.0]
(* val primer_vektorji_1 : float list = [2.; 4.; 6.] *)

(* !!! Torej List.map gre samo čez en seznam čez, medtem ko
!!!!!  List.map2 gre čez dva hkrati*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sestej : float list -> float list -> float list`, ki vrne
 vsoto dveh vektorjev.
*----------------------------------------------------------------------------*)

let sestej u v = List.map2(fun x y -> x +. y)u v

let sestej2 u v = List.map2( +.) u v
let primer_vektorji_2 = sestej [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_2 : float list = [5.; 7.; 9.] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `skalarni_produkt : float list -> float list -> float`, ki
 izračuna skalarni produkt dveh vektorjev. Pri tem si lahko pomagate s funkcijo
 `vsota_seznama : float list -> float`, definirano prek funkcije
 `List.fold_left`, ki jo bomo spoznali kasneje:
*----------------------------------------------------------------------------*)

let vsota_seznama = List.fold_left (+.) 0.

let skalarni_produkt u v = vsota_seznama(
  List.map2(fun x y -> x *. y) u v
)

let primer_vektorji_3 = skalarni_produkt [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]
(* val primer_vektorji_3 : float = 32. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `norma : float list -> float`, ki vrne evklidsko normo
 vektorja.
*[----------------------------------------------------------------------------*)

let norma v = sqrt (
  skalarni_produkt v v
)

let primer_vektorji_4 = norma [3.0; 4.0]
(* val primer_vektorji_4 : float = 5. *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vmesni_kot : float list -> float list -> float`, ki izračuna
 kot med dvema vektorjema v radianih.
*----------------------------------------------------------------------------*)

let vmesni_kot u v = acos(
  (skalarni_produkt u v) /. (norma u *. norma v)
)

let primer_vektorji_5 = vmesni_kot [1.0; 0.0] [0.0; 1.0]
(* val primer_vektorji_5 : float = 1.57079632679489656 *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `normirani : float list -> float list`, ki normira dani
 vektor.
*----------------------------------------------------------------------------*)

let normirani v = List.map(fun x -> x /. norma v) v

let primer_vektorji_6 = normirani [3.0; 4.0]
(* val primer_vektorji_6 : float list = [0.600000000000000089; 0.8] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `projeciraj : float list -> float list -> float list`, ki
 izračuna projekcijo prvega vektorja na drugega.
*----------------------------------------------------------------------------*)

let projekcija u v = List.map2(fun x y -> (skalarni_produkt u v) /. norma v *. y) u v

let primer_vektorji_7 = projekcija [3.0; 4.0] [1.0; 0.0]
(* val primer_vektorji_7 : float list = [3.; 0.] *)

(*----------------------------------------------------------------------------*
 ## Generiranje HTML-ja
*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML
 oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.
*----------------------------------------------------------------------------*)

let ovij oznaka niz = 
  "<" ^ oznaka ^ ">" ^ niz ^ "<" ^ oznaka ^ ">"
let primer_html_1 = ovij "h1" "Hello, world!"
(* val primer_html_1 : string = "<h1>Hello, world!</h1>" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število
 presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za
 ustrezno število presledkov.
*----------------------------------------------------------------------------*)

let zamakni k niz = 
  String.split_on_char '\n' niz
  |> List.map(fun vrstica -> String.make k ' ' ^ vrstica)
  |> String.concat "\n"

let primer_html_2 = zamakni 4 "Hello,\nworld!"
(* val primer_html_2 : string = "    Hello,\n    world!" *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne
 niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:
*----------------------------------------------------------------------------*)

let ul sez = 
  "<ul>\n" ^
  String.concat""(List.map(fun x -> " <li>" ^ x ^ " <li>\n")sez ) ^
  "<ul>"
 
let primer_html_3 = ul ["ananas"; "banana"; "čokolada"]
(* val primer_html_3 : string =
  "<ul>\n  <li>ananas</li>\n  <li>banana</li>\n  <li>čokolada</li>\n</ul>" *)

(*----------------------------------------------------------------------------*
 ## Nakupovalni seznam
*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme
 niz, ki vsebuje vejico, loči na del pred in del za njo.
*----------------------------------------------------------------------------*)

let razdeli_vrstico niz = 
  let vejica = String.index niz ',' in 
  let levi = String.sub niz 0 vejica in
  let desni = String.sub niz (vejica + 2)(String.length niz - 2) in
  levi, desni

let primer_seznam_1 = razdeli_vrstico "mleko, 2"
(* val primer_seznam_1 : string * string = ("mleko", "2") *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`,
 ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike `"izdelek,
 vrednost"`, in vrne seznam ustreznih parov.
*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz = 
  String.split_on_char '\n' niz
  |> List.map razdeli_vrstico

let primer_seznam_2 = pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"
(* val primer_seznam_2 : (string * string) list =
  [("mleko", "2"); ("kruh", "1"); ("jabolko", "5")] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list
 -> (string * 'b) list`, ki dano funkcijo uporabi na vseh drugih komponentah
 elementov seznama.
*----------------------------------------------------------------------------*)

let pretvori_druge_komponente f sez = 
  List.map(fun (a, b) -> a , f b) sez

let primer_seznam_3 =
  let seznam = [("ata", "mama"); ("teta", "stric")] in
  pretvori_druge_komponente String.length seznam
(* val primer_seznam_3 : (string * int) list = [("ata", 4); ("teta", 5)] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki
 sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni
 znesek nakupa.
*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek nakup cenik = 
  let urejen_seznam = pretvori_v_seznam_parov nakup
  |> pretvori_druge_komponente float_of_string in
  let urejen_cenik = pretvori_v_seznam_parov cenik
  |> pretvori_druge_komponente float_of_string in
  let cena_izdelka izdelek =
    List.map ((fun (i1, p1) (i2, p2) -> if i1 = i2 then p1 *. p2 else 0.) (fst izdelek, snd izdelek)) urejen_cenik
    |> vsota_seznama in 
    urejen_seznam 
    |> List.map cena_izdelka
    |> vsota_seznama

let primer_seznam_4 = 
  let nakupovalni_seznam = "mleko, 2\njabolka, 5"
  and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
  izracunaj_skupni_znesek cenik nakupovalni_seznam
(* val primer_seznam_4 : float = 5.5 *)
