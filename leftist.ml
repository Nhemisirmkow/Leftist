(*           Zadanie Leftist          *)
(* Autor: Marcin Michorzewski         *)
(* Code Review : Daniel Grzegorzewski *)
(* Tworzymy implementację zgodną z    *)
(* plikiem leftist.mli                *)


(*Definiujemy węzeł drzewa lewicowego *)
(*jako czwórkę (wartość, lewy syn,    *)
(*prawy syn, rpl - right path length) *)

type 'a queue =
  |Null
  |Node of 'a * 'a queue * 'a queue * int

let empty = Null                    (*konstruktor pustego drzewa*)

exception Empty         (*Wyjątek, gdy drzewo puste w delete_min*)

(*is_empty zwraca prawdę, gdy drzewo, które bada, nie jest puste*)

let is_empty (a : 'a queue) =
  match a with
    |Null -> true
    |Node (_, _, _, _) -> false

(*rpl zwraca najdłuższą ścieżkę idącą jedynie w prawo           *)

let rpl (a : 'a queue) =
  match a with
    |Null -> -1
    |Node (_, _, _, w) -> w

(*join a b łączy dwa drzewa a b. W tym celu, sprawdza czy jakieś*)
(*nie jest puste, jeżeli tak, wynikiem jest drugie drzewo.      *)
(*Ponadto zakładamy, że wartość w węźle a <= wartości w węźle b *)
(*Jeżeli tak nie jest, całą funkcję wywołujemy ponownie z zamie-*)
(*nionymi miejscami argumentami. Można to wykonać, bo funkcja   *)
(*join jest łączna. Wynikiem jest zatem drzewo, którego wartość *)
(*w korzeniu jest równa wartości w korzeniu lewego drzewa.      *)
(*Synami tego drzewa są lewe poddrzewo lewego drzewa oraz wynik *)
(*join od prawego poddrzewa lewego drzewa i prawego drzewa      *)
(*Otrzymany wynik, jest zatem zbudowany z poddrzew lewicowych   *)
(*(jeżeli tylko wynik jest drzewem lewicowym, co zachodzi dla   *)
(*trywialnych sum), dodatkowo, by był lewicowy musi spełniać    *)
(*warunek, że rpl prawego syna jest mniejsze/równe niż rpl lewe-*)
(*go syna, o co możemy zadbać gdy zamienimy synów miejscami,    *)
(*jeśli tak nie jest. A zatem wynik jest drzewem spełniającym   *)
(*warunki zadania                                               *)


let rec join (a : 'a queue) (b : 'a queue) =
  match a with
    |Null -> b
    |Node (wa, al, ar, _) -> 
      match b with
        |Null -> a
	|Node (wb, bl, br, _) ->
	  if wa <= wb
	  then
	    let q = join ar b
	    in
	      if rpl q >= rpl al then Node (wa, q, al, (rpl al) + 1)
	      else Node (wa, al, q, (rpl q) + 1)
	  else
	    join b a

(*add dodaje element do drzewa poprzez dodanie jednoelementowego*)
(*drzewa do naszego drzewa                                      *)

let add a (q : 'a queue) =
  join (Node (a, Null, Null, 0)) q
    
(*delete_min zwraca parę: (wartość w węźle, wynik sumy poddrzew *)
(*korzenia). Jeżeli korzeń nie istnieje zwracamy wyjątek        *)

let delete_min (q : 'a queue) =
  match q with
    |Null -> raise(Empty)
    |Node (a, b, c, _) -> (a, join b c)
