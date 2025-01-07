(* META MORPION *)
(* DEFINITION DES TYPES  *)

type player = J1 | J2
type cell = Pion of player * int | Vide
type grid = cell array array
type grille_equivalente = player option array array
type etat = { grid: grid ; player: player ; grande_case : int * int ; ge : grille_equivalente ; numero : int}

(* FONCTIONS DE BASE *)

(* joueur dont c'est le tour *)
let joueur (e: etat): player = e.player

(* copie une grille *)
let copier_matrice (matrice: cell array array): cell array array =
  let nb_lignes = Array.length matrice in
  let nb_colonnes = Array.length matrice.(0) in
  let nouvelle_matrice = Array.make_matrix nb_lignes nb_colonnes Vide (* initialiser avec des valeurs par défaut *) in
  for i = 0 to nb_lignes - 1 do
    for j = 0 to nb_colonnes - 1 do
      nouvelle_matrice.(i).(j) <- matrice.(i).(j)
    done
  done;
  nouvelle_matrice

(* copie une grille equivalente *)
let copier_matrice_2 (matrice: player option array array): player option array array =
  let nb_lignes = Array.length matrice in
  let nb_colonnes = Array.length matrice.(0) in
  let nouvelle_matrice = Array.make_matrix nb_lignes nb_colonnes None (* initialiser avec des valeurs par défaut *) in
  for i = 0 to nb_lignes - 1 do
    for j = 0 to nb_colonnes - 1 do
      nouvelle_matrice.(i).(j) <- matrice.(i).(j)
    done
  done;
  nouvelle_matrice

(* renvoie le joueur ayant joué dans la case i j *)
let get_player (g: grid)(i:int)(j:int): player option = 
  match g.(i).(j) with
  |Vide -> None
  |Pion(j,n) -> Some(j)

(* renvoie la grande case i j sous forme de tableau option *)
let get_case (e:etat)(i: int)(j:int): player option array array =
  let t = Array.make_matrix 3 3 None in
  for k=0 to 2 do
    for l=0 to 2 do
      t.(k).(l) <- get_player e.grid (3*i + k) (3*j +l)
    done;
  done;
  t

(* donne l'autre joueur *)
let autre j = match j with J1 -> J2 | J2 -> J1

(* initialise un etat qui commence dans la grande case i j *)
let init i j = { grid = Array.make_matrix 9 9 Vide ; player = J1 ; grande_case = (i,j) ; ge = Array.make_matrix 3 3 None; numero = 0}

(* renvoie le gagnant de l'état s'il y en a un none sinon *)
let est_gagnant (e: etat): player option =
  let (i,j)=(0,0) in
  if e.ge.(i).(j) = e.ge.(i+1).(j+1) && e.ge.(i).(j) = e.ge.(i+2).(j+2) then 
    e.ge.(i).(j) (* diagonale *)
  else if e.ge.(i+2).(j) = e.ge.(i+1).(j+1) && e.ge.(i+2).(j) = e.ge.(i).(j+2) then
    e.ge.(i+1).(j+1) (* autre diagonale *)
  else if e.ge.(i).(j) = e.ge.(i+1).(j) && e.ge.(i).(j) = e.ge.(i+2).(j) then
    e.ge.(i).(j) (* ligne 1 *)
  else if e.ge.(i).(j+1) = e.ge.(i+1).(j+1) && e.ge.(i).(j+1) = e.ge.(i+2).(j+1) then
    e.ge.(i).(j+1) (* ligne 2 *)
  else if e.ge.(i).(j+2) = e.ge.(i+1).(j+2) && e.ge.(i).(j+2) = e.ge.(i+2).(j+2) then
    e.ge.(i).(j+2) (* ligne 3 *)
  else if e.ge.(i).(j) = e.ge.(i).(j+1) && e.ge.(i).(j) = e.ge.(i).(j+2) then 
    e.ge.(i).(j) (* colonne 1 *)
  else if e.ge.(i+1).(j) = e.ge.(i+1).(j+1) && e.ge.(i+1).(j) = e.ge.(i+1).(j+2) then 
    e.ge.(i+1).(j) (* colonne 2 *)
  else if e.ge.(i+2).(j) = e.ge.(i+2).(j+1) && e.ge.(i+2).(j) = e.ge.(i+2).(j+2) then 
    e.ge.(i+2).(j) (* colonne 3 *)
  else None

(* retourne le joueur gagnant la grande case d'indice i j none sinon*)
let case_gagnee (g: grid)(i: int)(j: int): player option =
  if get_player g (3*i) (3*j) = get_player g (3*i+1) (3*j+1) && get_player g (3*i) (3*j) = get_player g (3*i+2) (3*j+2) then 
    get_player g (3*i) (3*j) (* diagonale *)
  else if get_player g (3*i+2) (3*j) = get_player g (3*i+1) (3*j+1) && get_player g (3*i+2) (j) = get_player g (3*i) (3*j+2) then
    get_player g (3*i+1) (3*j+1) (* autre diagonale *)
  else if get_player g (3*i) (3*j) = get_player g (3*i+1) (3*j) && get_player g (3*i) (3*j) = get_player g (3*i+2) (3*j) then
    get_player g (3*i) (3*j) (* ligne 1 *)
  else if get_player g (3*i) (3*j+1) = get_player g (3*i+1) (3*j+1) && get_player g (3*i) (3*j+1) = get_player g (3*i+2) (3*j+1) then
    get_player g (3*i) (3*j+1) (* ligne 2 *)
  else if get_player g (3*i) (3*j+2) = get_player g (3*i+1) (3*j+2) && get_player g (3*i) (3*j+2) = get_player g (3*i+2) (3*j+2) then
    get_player g (3*i) (3*j+2) (* ligne 3 *)
  else if get_player g (3*i) (3*j) = get_player g (3*i) (3*j+1) && get_player g (3*i) (3*j) = get_player g (3*i) (3*j+2) then 
    get_player g (3*i) (3*j) (* colonne 1 *)
  else if get_player g (3*i+1) (3*j) = get_player g (3*i+1) (3*j+1) && get_player g (3*i+1) (3*j) = get_player g (3*i+1) (3*j+2) then 
    get_player g (3*i+1) (3*j) (* colonne 2 *)
  else if get_player g (3*i+2) (3*j) = get_player g (3*i+2) (3*j+1) && get_player g (3*i+2) (3*j) = get_player g (3*i+2) (3*j+2) then 
    get_player g (3*i+2) (3*j) (* colonne 3 *)
  else None

(* donne l'etat apres jeu dans la case i j *)
let joue (e:etat)(c : int * int)(numero : int) : etat =
  let (i,j) = c in 
  if e.grid.(i).(j)= Vide then
    let copie = copier_matrice e.grid in
    let ce = copier_matrice_2 e.ge in
    copie.(i).(j) <- Pion(e.player,e.numero);
    if e.ge.(i/3).(j/3) = None then
      ce.(i/3).(j/3) <- case_gagnee copie (i/3) (j/3);
    {grid=copie; player= autre e.player ; grande_case =(i mod 3,j mod 3) ; ge = ce ; numero = e.numero + 1}
  else
    failwith("Case occupée")

(* donne la liste des etats accessibles depuis un etat *)
let possible_moves (e: etat): etat list =
(*   if(est_gagnant e != None) then failwith ("bravo")
  else *)
  let liste_rep = ref [] in
  let (i,j) = e.grande_case in
  let rec aux e i j =
    let case = get_case e i j in
    if Array.exists (Array.exists (fun x -> x = None)) case then
      (for k=0 to 2 do
        for l=0 to 2 do
          if case.(k).(l)=None then
            liste_rep := (joue e ((3*i+k),(3*j+l)) (e.numero+1))::(!liste_rep)
        done;
      done;
      !liste_rep)
    else
      if i=2 && j=2 then
        aux e 0 0
      else if j=2 then
        aux e (i+1) 0
      else aux e i (j+1)
  in aux e i j

(* donne les coordonnees de la case du dernier coup joué *)
let trouve_dernier_coup (g : grid): int * int =
  let max = ref 0 in
  let argmax = ref (0,0) in
  for i = 0 to 8 do
    for j = 0 to 8 do
      match g.(i).(j) with
      |Vide      -> ()
      |Pion(p,q) -> if q > (!max) then (argmax := (i,j); max := q)
    done
  done;
  !argmax


(* MISE EN PLACE DE STRATEGIES *)
(* MONTE CARLO TREE SEARCH *)

(* Définition des types *)
(* type etat = {
  grid: grid ; (* grille du meta morpion *)
  player: player ; (* J1 ou J2 dont c'est le tour de jouer *)
  grande_case : int * int ;
  (* coordonnées  de la case dans laquelle le joueur dont c'est le tour doit jouer *)
  ge : grille_equivalente ; (* grille 3x3 des gagnants de chaque case *)
  numero : int (* numéro du coup à jouer *)
} *)
type move = int*int*int
type node = {
  play : move; (* mvt effectué pour arriver ici *)
  state : etat;
  mutable wins : float;
  mutable visits : int; (* nombre de visites du noeud déjà effectuées *)
  parent : node option; (* noeud parent s'il existe None si c'est la racine *)
  unexpanded_plays : move list;
  mutable children : move * node list; (* liste des noeuds enfants *)
}

(* Génère et retourne l'état initial *)
let start : etat = init 1 1

(* Retourne les mouvements possibles pour le joueur *)
let legal_plays (e:etat): move list =
  (*   if(est_gagnant e != None) then failwith ("bravo")
  else *)
  let liste_rep = ref [] in
  let (i,j) = e.grande_case in
  let rec aux e i j =
    let case = get_case e i j in
    if Array.exists (Array.exists (fun x -> x = None)) case then
      (for k=0 to 2 do
        for l=0 to 2 do
          if case.(k).(l)=None then
            liste_rep := ((3*i+k),(3*j+l),(e.numero+1))::(!liste_rep)
        done;
      done;
      !liste_rep)
    else
      if i=2 && j=2 then
        aux e 0 0
      else if j=2 then
        aux e (i+1) 0
      else aux e i (j+1)
  in aux e i j

(* retourne le gagnant du jeu *)
let winner (e: etat): player option =
  est_gagnant e

(* Avance l'état selon le coup donné *)
let next_state (e: etat)(move: int*int*int): etat =
  let (i,j,k) = move in
  joue e (i,j) k

(* Sélection d'un noeud *)
let ucb1 (c: float)(child: node): float =
  let exploit = child.wins /. float_of_int child.visits in
  let explore = c *. sqrt (log (float_of_int (match child.parent with Some p -> p.visits | None -> 1)) /. float_of_int child.visits) in
  exploit +. explore

(* répète mcts depuis un état donné *)
let run_search (e: etat)(timeout: int): () =

(* donne le meilleur coup selon les statistiques *)
let best_play (e: etat): (int*int*int) =

(* renvoie le noeud correspondant à ce mouvement *)
let child_node (play: move): node =

(*  *)
let all_plays : move list =

(*  *)
let unexpanded_plays

(* Expansion *)

(* Simulation *)

(* Rétro propagation *)

(* Boucle principale *)

(* Utilisation *)




(* MIN-MAX *)
type int_barre =
  | Pinf (* +∞ *)
  | Minf (* −∞ *)
  | Fini of int (* n∈N *)

(* Fonction pour évaluer une sous-grille *)
let evaluation_grille (g: grid)(i: int)(j: int)(p: player) : int =
  match case_gagnee g i j with
  | Some player when player = p -> 1000
  | Some _ -> -1000
  | None ->
    let score = ref 0 in
    (* Points pour alignements de deux pions *)
    for i = 0 to 2 do
      for j = 0 to 2 do
        match g.(i).(j) with
        | Pion (player, _) when player = p ->
          score := !score + (if i = 1 && j = 1 then 3 else if i = 0 || i = 2 || j = 0 || j = 2 then 1 else 0)
        | _ -> ()
      done
    done;
    !score

(* Fonction pour évaluer le plateau *)
let heuristique (p:player)(e:etat): int =
  let score = ref 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      match e.ge.(i).(j) with
      | Some player when player = p -> score := !score + 1000
      | Some _ -> score := !score - 1000
      | None ->
        let sub_score = evaluation_grille e.grid i j e.player in
        score := !score + sub_score * (if i = 1 && j = 1 then 30 else if i = 0 || i = 2 || j = 0 || j = 2 then 10 else 1)
    done
  done;
  (!score)

(* Exception levée si la liste est vide *)
exception Empty_list

(* retourne le max d'une liste *)
let max_list lst =
  match lst with
  | [] -> raise Empty_list  (* Lever une exception si la liste est vide *)
  | hd :: tl -> 
      let rec aux current_max = function
        | [] -> current_max
        | x :: xs -> aux (if x > current_max then x else current_max) xs
      in
      aux hd tl
(* retourne le minimum d'une liste *)
let min_list lst =
  match lst with
  | [] -> raise Empty_list  (* Lever une exception si la liste est vide *)
  | hd :: tl -> 
      let rec aux current_min = function
        | [] -> current_min
        | x :: xs -> aux (if x < current_min then x else current_min) xs
      in
      aux hd tl


let rec minmax (e: etat)(j: player)(prof_max: int): int_barre =
  if List.is_empty (possible_moves e) then
    if est_gagnant e = Some(j) then
      Pinf
    else if est_gagnant e = Some(autre j) then
      Minf
    else Fini(0)
  else if prof_max = 0 then
    Fini(heuristique j e)
  else if e.player = j then
    max_list (List.map (fun x -> minmax x j (prof_max-1)) (possible_moves e))
  else 
    min_list (List.map (fun x -> minmax x j (prof_max-1)) (possible_moves e))



let meilleur_coup (e: etat)(d: int): etat = 
  let rec aux l_etats meilleur_etat = (* trouve récursivement l'etat maximisant l'heuristique *)
    match l_etats with
    |[]   -> meilleur_etat
    |x::q -> 
      if (minmax x e.player d) > (minmax meilleur_etat e.player d) then
        aux q meilleur_etat
      else
        aux q x
  in aux (possible_moves e) (List.hd (possible_moves e))

(* fonction de comparaison des int_barre *)
let plus_grand (a: int_barre)(b: int_barre): int_barre =
  match (a,b) with
  |Pinf,_         -> Pinf
  |_,Pinf         -> Pinf
  |Minf,Fini(x)   -> Fini(x)
  |Fini(x),Minf   -> Fini(x)
  |Fini(x),Fini(y)-> Fini(max x y)
  |Minf,Minf      -> Minf

let plus_petit (a: int_barre)(b: int_barre): int_barre =
  if (plus_grand a b) = a then b else a  

(* élagage alpha beta *)
let rec minmax_elagage (e: etat)(j: player)(d: int)(alpha: int_barre)(beta: int_barre): int_barre =
  if List.is_empty (possible_moves e) then
    if est_gagnant e = Some(j) then
      Pinf
    else if est_gagnant e = Some(autre j) then
      Minf
    else Fini(0)
  else if d = 0 then
    Fini(heuristique j e)
  else if e.player = j then
    begin
      let v = ref Minf in
      let m = max_list (List.map (fun x -> minmax_elagage x j (d-1) !v Pinf) (possible_moves e)) in
      v := plus_grand !v m;
      if (plus_grand !v beta) = !v then !v else beta
    end
  else 
    begin
      let v = ref Minf in
      let m = min_list (List.map (fun x -> minmax_elagage x j (d-1) Minf !v) (possible_moves e)) in
      v := plus_petit !v m;
      if (plus_petit !v alpha) = !v then !v else alpha 
    end

let meilleur_coup_2 (e: etat)(d: int)(alpha: int_barre)(beta: int_barre): etat = 
  let rec aux l_etats meilleur_etat = (* trouve récursivement l'etat maximisant l'heuristique *)
    match l_etats with
    |[]   -> meilleur_etat
    |x::q -> 
      if (minmax_elagage x e.player d alpha beta) > (minmax_elagage meilleur_etat e.player d alpha beta) then
        aux q meilleur_etat
      else
        aux q x
  in aux (possible_moves e) (List.hd (possible_moves e))


(* JOUEUR ALÉATOIRE *)

let random_move (e: etat)(k: int): etat =
  let (a,b) = e.grande_case in
  let i = Random.int 3 in
  let j = Random.int 3 in
  joue e (3*a+i,3*b+j) k



(* AFFICHAGE *)

(* Fonction pour convertir une cellule en chaîne de caractères *)
let cell_to_string = function
  | Vide -> " "
  | Pion (J1, _) -> "X"
  | Pion (J2, _) -> "O"

(* Fonction pour afficher une grille *)
let print_grid (grid: grid) =
  let size = 9 in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      print_string (cell_to_string grid.(i).(j));
      if j < size - 1 then print_string " | "
    done;
    print_newline ();
    if i < size - 1 then print_endline (String.make (4 * size - 1) '-')
  done

(* affiche le gagnant *)
let string_of_player (p: player): string = 
  match p with
  |J1 -> "X"
  |J2 -> "O"

let afficher_gagnant (e: etat) =
  match est_gagnant e with
  | Some player -> Printf.printf "Le gagnant est: %s\n" (string_of_player player)
  | None -> Printf.printf "Pas de gagnant pour le moment.\n"





(* TESTS  *)

let next_move (): int * int =
  print_endline "ligne : ";
  let user_input = input_line stdin in
  let i = int_of_string user_input in
  print_endline "colonne : ";
  let user_input = input_line stdin in
  let j = int_of_string user_input in
  (i,j)

(* let grille_test = Array.make_matrix 9 9 Vide
let ge_test = Array.make_matrix 3 3 None
let rep = ref (init 1 1) *)

let () =
  (* let d = 5 in *)
  let e = ref (init 1 1) in
  let k = ref 0 in

  (* mcts vs moi *)
  mcts root 200 1.41;  (* 10000 itérations et C = 1.41 pour l'UCT *)
  let best_move = ref (best_child root) in
  (* Utiliser best_move pour décider du prochain mouvement *)
  while ((est_gagnant (!e)) = None) && ((!k)< 81) do
    begin
    print_grid (!e).grid;
    e := (!best_move).state;
    k := !k +1;
    print_grid (!e).grid;
    let c = next_move() in
    e := joue (!e) c (!k);
    k := !k +1;
    best_move := { state = (!e); wins = 0.0; visits = 0; parent = None; children = [] };
    mcts (!best_move) 200 1.41;
    best_move := best_child (!best_move)
    end
  done


  (* minmax vs minmax elagage heuristique 1 *)
  (* while ((est_gagnant (!e)) = None) && ((!k)< 81) do
    begin
    e := meilleur_coup_2 !e d Minf Pinf;
    k := !k +1;
    print_grid (!e).grid;
    e := meilleur_coup !e d;
    k := !k +1;
    print_grid (!e).grid;
    end
  done;
  afficher_gagnant !e
   *)
  (* heuristique 1 minmax contre soi *)
  (* while ((est_gagnant (!e)) = None) && ((!k)< 81) do
    begin
    let c = next_move() in
    e := joue (!e) c (!k);
    k := !k +1;
    e := meilleur_coup !e d;
    k := !k +1;
    print_grid (!e).grid;
    end
  done *)

  (* heuristique 1 min max contre le nul *)
  (* while ((est_gagnant (!e)) = None) && ((!k)< 81) do
    begin
    e := List.hd (possible_moves !e);
    k := !k +1;
    e := meilleur_coup !e d;
    k := !k +1;
    print_grid (!e).grid;
    end
  done *)

  (* soi contre le nul *)
  (* while ((est_gagnant (!e)) = None) && ((!k)< 81) do
    begin
    e := List.hd (possible_moves !e);
    k := !k +1;
    print_grid (!e).grid;
    let c = next_move() in
    e := joue (!e) c (!k);
    k := !k +1;
    end
  done *)



