(* META MORPION *)


(* DEFINITION DES TYPES  *)

type player = J1 | J2
type cell = Pion of player * int | Vide
type grid = cell array array
type grille_equivalente = player option array array
type etat = {
  mutable grid: grid ;
  mutable player: player ;
  mutable grande_case : int * int ;
  mutable ge : grille_equivalente ;
  mutable numero : int}

(* FONCTIONS DE BASE *)

(* joueur dont c'est le tour *)
let joueur (e: etat): player = e.player

(* copie une grille *)
let copier_matrice (matrice: cell array array): cell array array =
  let nb_lignes = Array.length matrice in
  let nb_colonnes = Array.length matrice.(0) in
  let nouvelle_matrice = Array.make_matrix nb_lignes nb_colonnes Vide
  (* initialiser avec des valeurs par défaut *) in
  for i = 0 to nb_lignes - 1 do
    for j = 0 to nb_colonnes - 1 do
      nouvelle_matrice.(i).(j) <- matrice.(i).(j)
    done
  done;
  nouvelle_matrice
(* copie une grille equivalente *)
let copier_matrice_2 (matrice: player option array array) =
  let nb_lignes = Array.length matrice in
  let nb_colonnes = Array.length matrice.(0) in
  let nouvelle_matrice = Array.make_matrix nb_lignes nb_colonnes None in
  (* initialiser avec des valeurs par défaut *)
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
let init i j = { grid = Array.make_matrix 9 9 Vide ; player = J1 ;
grande_case = (i,j) ; ge = Array.make_matrix 3 3 None; numero = 0}

(* renvoie le gagnant de l'état s'il y en a un none sinon *)
let est_gagnant (e: etat): player option =
  let u = e.ge in
  (* Pré-calcul des valeurs *)
  let a = u.(0).(0) in
  let b = u.(1).(0) in
  let c = u.(2).(0) in
  let d = u.(0).(1) in
  let e = u.(1).(1) in
  let f = u.(2).(1) in
  let g = u.(0).(2) in
  let h = u.(1).(2) in
  let i = u.(2).(2) in

  if a <> None && a = b && b = c then a (* Colonne 1 *)
  else if d <> None && d = e && e = f then d (* Colonne 2 *)
  else if g <> None && g = h && h = i then g (* Colonne 3 *)
  else if a <> None && a = d && d = g then a (* Ligne 1 *)
  else if b <> None && b = e && e = h then b (* Ligne 2 *)
  else if c <> None && c = f && f = i then c (* Ligne 3 *)
  else if a <> None && a = e && e = i then a (* Diagonale principale *)
  else if c <> None && c = e && e = g then c (* Diagonale secondaire *)
  else None


(* retourne le joueur gagnant la grande case d'indice i j, None sinon*)
let case_gagnee (g: grid)(i: int)(j: int): player option =
  if get_player g (3*i) (3*j) <> None && get_player g (3*i) (3*j) = get_player g (3*i+1) (3*j+1) && get_player g (3*i) (3*j) = get_player g (3*i+2) (3*j+2) then 
    get_player g (3*i) (3*j) (* diagonale *)
  else if get_player g (3*i+2) (3*j) <> None && get_player g (3*i+2) (3*j) = get_player g (3*i+1) (3*j+1) && get_player g (3*i+2) (j) = get_player g (3*i) (3*j+2) then
    get_player g (3*i+1) (3*j+1) (* autre diagonale *)
  else if get_player g (3*i) (3*j) <> None && get_player g (3*i) (3*j) = get_player g (3*i+1) (3*j) && get_player g (3*i) (3*j) = get_player g (3*i+2) (3*j) then
    get_player g (3*i) (3*j) (* ligne 1 *)
  else if get_player g (3*i) (3*j+1) <> None && get_player g (3*i) (3*j+1) = get_player g (3*i+1) (3*j+1) && get_player g (3*i) (3*j+1) = get_player g (3*i+2) (3*j+1) then
    get_player g (3*i) (3*j+1) (* ligne 2 *)
  else if get_player g (3*i) (3*j+2) <> None && get_player g (3*i) (3*j+2) = get_player g (3*i+1) (3*j+2) && get_player g (3*i) (3*j+2) = get_player g (3*i+2) (3*j+2) then
    get_player g (3*i) (3*j+2) (* ligne 3 *)
  else if get_player g (3*i) (3*j) <> None && get_player g (3*i) (3*j) = get_player g (3*i) (3*j+1) && get_player g (3*i) (3*j) = get_player g (3*i) (3*j+2) then 
    get_player g (3*i) (3*j) (* colonne 1 *)
  else if get_player g (3*i+1) (3*j) <> None && get_player g (3*i+1) (3*j) = get_player g (3*i+1) (3*j+1) && get_player g (3*i+1) (3*j) = get_player g (3*i+1) (3*j+2) then 
    get_player g (3*i+1) (3*j) (* colonne 2 *)
  else if get_player g (3*i+2) (3*j) <> None && get_player g (3*i+2) (3*j) = get_player g (3*i+2) (3*j+1) && get_player g (3*i+2) (3*j) = get_player g (3*i+2) (3*j+2) then 
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
    {grid=copie; player= autre e.player ; grande_case =(i mod 3,j mod 3) ;
    ge = ce ; numero = e.numero + 1}
  else
    failwith("Case occupée")

(* renvoie le symbole associé au joueur *)
let string_of_player_option (p: player option): string = 
  match p with
  |Some(j) -> if j = J1 then "X" else "O"
  |None -> "  "

(* affiche une grille *)
let affiche_grande_case (c: player option array array)=
  for i=0 to 2 do
    for j=0 to 2 do
      print_string (string_of_player_option c.(i).(j));
      if j < 2 then print_string " | "
    done;
    print_newline ();
    if i < 2 then print_endline (String.make (13) '-')
  done

let cell_to_string = function
  | Vide -> " "
  | Pion (J1, _) -> "X"
  | Pion (J2, _) -> "O"

(* affiche le plateau *)
let print_grid (grid: grid) =
  let size = 9 in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      print_string (cell_to_string grid.(i).(j));
      let num = match grid.(i).(j) with 
      |Vide -> -1
      |Pion(j,n) -> n
      in 
      if (num <> -1) then print_int num else print_string " " ;
      if j < size - 1 then print_string " | "
    done;
    print_newline ();
    if i < size - 1 then print_endline (String.make ((5 * size - 1) - 1) '-')
  done;
  print_newline()

(* donne la liste des etats accessibles depuis un etat *)
let possible_moves (e: etat): etat list =
  let moves = ref [] in
  let add_move k l i j case =
    if case.(k).(l) = None then
      (let new_move = joue e (3*i + k, 3*j + l) (e.numero + 1) in
      moves := new_move :: !moves)
    in
    let rec aux i j =
      let case = get_case e i j in
      if Array.exists (Array.exists (fun x -> x = None)) case then begin
        for k = 0 to 2 do
          for l = 0 to 2 do
            add_move k l i j case
          done
        done;
        !moves
      end
    else
      if i = 2 && j = 2 then aux 0 0
      else if j = 2 then aux (i + 1) 0
      else aux i (j + 1)
    in
    let (i, j) = e.grande_case in
  aux i j


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
(* MIN-MAX *)
type int_barre =
  | Pinf (* +∞ *)
  | Minf (* −∞ *)
  | Fini of int (* n∈N *)

(* HEURISTIQUE 1 *)
(* Fonction pour évaluer une sous-grille *)
let evaluation_grille (g: grid)(i: int)(j: int)(p: player) : int =
  match case_gagnee g i j with
  | Some player when player = p -> 1000
  | Some _ -> -1000
  | None ->
    let score = ref 0 in
    (* Points pour alignements de deux pions *)
    for k = 0 to 2 do
      for l = 0 to 2 do
        match g.(3*i+k).(3*j+l) with
        | Pion (player, _) when player = p ->
          score := !score + (if k = 1 && l = 1 then 4
          else if k = 0 || k = 2 || l = 0 || l = 2 then 2 else 1)
        | _ -> ()
      done
    done;
    print_string ("score de la grille : ");
    print_int(!score);
    print_newline();
    flush stdout;
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
        let sub_score = evaluation_grille e.grid i j p in
        score := !score + sub_score * (if i = 1 && j = 1 then 30
        else if i = 0 || i = 2 || j = 0 || j = 2 then 10 else 1)
    done
  done;
  (!score)

(* HEURISTIQUE 2 *)
(* donne les coordonnées de la grille dans laquelle se trouve la case *)
let grille_de_case (case: int * int): int * int = 
  let (i,j) = case in (i/3, j/3)

(* trouve la case dans laquelle jouera le joueur suivant après le coup dans cette case *)
let grille_suivante_de_case (case: int * int): int * int = 
  let (i,j) = case in (i mod 3, j mod 3)

(* la grille i j est-elle gagnable par le joueur p en 1 coup ? *)
let gagnable (grille: int * int)(e: etat)(p: player): bool =
  let u = e.grid in
  let b = ref false in 
  let (i, j) = grille in
  for k=0 to 2 do
    for l=0 to 2 do
      if u.(3*i+k).(3*j+l) = Vide then
        if est_gagnant (joue e (3*i+k, 3*j+l) 0) = Some(p) then
          b := true
        else ()
      done;
    done;
  !b

let heuristique_v2 (p:player)(e:etat)(e_precedent:etat): int = 
  let score = ref 0 in
  let (i,j) = trouve_dernier_coup e.grid in
  (* points pour le coup actuel *)
  (* si le coup fait gagner la case *)
  if (e.ge.(i/3).(j/3)) = Some(p) && (e_precedent.ge.(i/3).(j/3)) = None then
    score := !score + 10000;
  (* si le coup crée un alignement *)
  if gagnable (grille_de_case (i,j)) e p && not (gagnable (grille_de_case (i,j)) e_precedent p) then
    score := !score + 100;
  if i mod 3 = 1 && j mod 3 = 1 then
    score := !score + 30
  else if (i mod 3 = 1 && j mod 3 <> 1) || (j mod 3 = 1 && i mod 3 <> 1) then ()
  else score := !score + 20;

  (* points pour les potentiels coups du joueur suivant *)
  (* s'il peut gagner la case dans laquelle il est envoyé *)
  if gagnable (grille_suivante_de_case (i,j)) e (autre p) then
    score := !score -1000
  else if gagnable (grille_suivante_de_case (i,j)) e p then
    score := !score -100;
  !score

(* Exception levée si la liste est vide *)
exception Empty_list

(* retourne le max d'une liste *)
let max_list lst =
  match lst with
  | [] -> raise Empty_list  (* Lève une exception si la liste est vide *)
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

(* MinMax *)
let rec minmax (e: etat)(j: player)(prof_max: int)(h : player->etat->int): int_barre =
  if List.is_empty (possible_moves e) then
    if est_gagnant e = Some(j) then Pinf
    else if est_gagnant e = Some(autre j) then Minf
    else Fini(0)
  else if prof_max = 0 then
    Fini(h j e)
  else if e.player = j then
    max_list (List.map (fun x -> minmax x j (prof_max-1) h) (possible_moves e))
  else 
    min_list (List.map (fun x -> minmax x j (prof_max-1) h) (possible_moves e))

(* meilleur coup à jouer d'après MinMax *)
let meilleur_coup (e: etat)(d: int)(h : player->etat->int): etat = 
  let rec aux l_etats meilleur_etat = (* trouve récursivement l'etat maximisant l'heuristique *)
    match l_etats with
    |[]   -> meilleur_etat
    |x::q -> 
      if (minmax x e.player d h) > (minmax meilleur_etat e.player d h) then
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

(* MinMax avec élagage alpha beta *)
let rec minmax_elagage (e: etat)(j: player)(d: int)(alpha: int_barre)(beta: int_barre)(h : player->etat->int): int_barre =
  if List.is_empty (possible_moves e) then
    if est_gagnant e = Some(j) then Pinf
    else if est_gagnant e = Some(autre j) then Minf
    else Fini(0)
  else if d = 0 then Fini(h j e)
  else if e.player = j then
    begin
      let v = ref Minf in
      let m = max_list (List.map (fun x -> minmax_elagage x j (d-1) !v Pinf h)
      (possible_moves e)) in
      v := plus_grand !v m;
      if (plus_grand !v beta) = !v then !v else beta
    end
  else 
    begin
      let v = ref Minf in
      let m = min_list (List.map (fun x -> minmax_elagage x j (d-1) Minf !v h)
      (possible_moves e)) in
      v := plus_petit !v m;
      if (plus_petit !v alpha) = !v then !v else alpha 
    end

(* meilleur coup à jouer d'après MinMax élagage *)
let meilleur_coup_2 (e: etat)(d: int)(alpha: int_barre)(beta: int_barre)
(h : player->etat->int): etat = 
  let rec aux l_etats meilleur_etat =
    (* trouve récursivement l'etat maximisant l'heuristique *)
    match l_etats with
    |[]   -> meilleur_etat
    |x::q -> 
      if (minmax_elagage x e.player d alpha beta h) > (minmax_elagage meilleur_etat e.player d alpha beta h) then
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

let string_of_player (p: player): string = 
  match p with
  |J1 -> "X"
  |J2 -> "O"

let afficher_gagnant (e: etat) =
  match est_gagnant e with
  | Some player -> Printf.printf "Le gagnant est: %s\n" (string_of_player player)
  | None -> Printf.printf "Pas de gagnant pour le moment.\n"



(* MONTE CARLO TREE SEARCH *)

(* Définition des types *)
type node = {
  state : etat;
  mutable wins : float;
  mutable visits : int;
  parent : node option;
  mutable children : node list;
}

(* Sélection d'un noeud *)
let ucb1 (c: float)(child: node): float =
  let exploit = child.wins /. float_of_int child.visits in
  let explore = c *. sqrt 
    (log (float_of_int 
    (match child.parent with Some p -> p.visits | None -> 1))
    /. float_of_int child.visits) in
  exploit +. explore

let rec select (node: node)(c: float): node =
  if node.children = [] then node
  else
    List.fold_left (fun best_child child ->
      if (ucb1 c child) > (ucb1 c best_child) then child else best_child
    ) (List.hd node.children) node.children

(* Expansion *)
let expand (node: node) =
  let new_states = possible_moves node.state in
  let new_nodes = List.map (fun s -> { state = s; wins = 0.0; visits = 0; parent = Some node; children = [] }) new_states in
  node.children <- new_nodes;
  List.hd new_nodes


(* Simulation *)
let random_next_state state =
  let moves = possible_moves state in
  if moves = [] then
    failwith("plus de mouvements") (* état terminal, aucun mouvement possible *)
  else
    let rand_index = Random.int (List.length moves) in
    let move = List.nth moves rand_index in
    move

let rec simulate (state: etat): float =
  if (est_gagnant state)=None && state.numero!=81 then
    let next_state = random_next_state state in
    simulate next_state
  else
    if (est_gagnant state)=Some(state.player) then 1.0 (* victoire *)
    else -1.0
    
(* Rétro propagation *)
let rec backpropagate (node: node)(result: float): unit =
  node.visits <- node.visits + 1;
  node.wins <- node.wins +. result;
  match node.parent with
  | Some parent -> backpropagate parent result
  | None -> ()

(* Boucle principale *)
let rec mcts (root: node)(iterations: int)(c: float): unit =
  for i = 1 to iterations do
    let selected_node = select root c in
    let expanded_node = expand selected_node in 
    let result = simulate expanded_node.state in
    backpropagate expanded_node result;
  done

(* meilleur coup à jouer d'après mcts *)
let best_child (root: node): node =
  List.fold_left (fun best_child child ->
    if child.visits > best_child.visits then child else best_child
  ) (List.hd root.children) root.children

(* trouve le noeud fils dans lequel se trouve cet état depuis un état précédent *)
let get_node (e: etat)(noeud_precedent: node): node =
  let enfants = noeud_precedent.children in
  let l' = List.filter (fun x -> x.state = e) enfants in
  match l' with
  |[] -> failwith ("pas d'état correspondant")
  |x::q -> if q = [] then x else failwith("plusieurs noeuds correspondent")


(* Utilisation *)
let initial_state = init 1 1
let root = { state = initial_state; wins = 0.0; visits = 0; parent = None; children = [] }


(* TESTS  *)

(* fonction pour jouer soi-même *)
let next_move (): int*int =
  print_endline "ligne : ";
  let user_input = input_line stdin in
  let i = int_of_string user_input in
  print_endline "colonne : ";
  let user_input = input_line stdin in
  let j = int_of_string user_input in (i,j)

(* Fonction pour jouer un coup avec MCTS *)
let jouer_coup_avec_mcts (root: node)(iterations: int)(c: float): etat =
  (* Exécuter MCTS *)
  mcts root iterations c;
  (* Trouver le meilleur coup *)
  let meilleur_noeud = best_child root in
  (* Appliquer l'état du meilleur coup *)
  (* appliquer_etat !e *)meilleur_noeud.state

(* Fonction pour afficher un noeud *)
let rec print_node (n: node) =
  print_endline "Etat du noeud :";
  print_endline ("Victoires : " ^ string_of_float n.wins);
  print_endline ("Visites : " ^ string_of_int n.visits);
  (match n.parent with
  | Some _ -> print_endline "Parent : Oui"
  | None -> print_endline "Parent : Non");
  print_endline ("Nombre d'enfants : " ^ string_of_int (List.length n.children));
  List.iter print_node n.children

(* Lancement du programme*)
let choisir_adversaire () =
  print_endline "Choisissez votre adversaire :";
  print_endline "1 - IA MCTS";
  print_endline "2 - IA MinMax";
  print_endline "3 - Joueur humain";
  print_string "Votre choix : ";
  read_int ()

let choisir_premier () =
  print_endline "Souhaitez-vous commencer ? (1 - Oui, 2 - Non)";
  print_string "Votre choix : ";
  read_int () = 1

let () =
  let adversaire = choisir_adversaire () in
  let joueur_commence = ref (choisir_premier ()) in

  let e = ref (init 1 1) in
  let k = ref 0 in
  let d = 4 in
  let root = ref { state = !e; wins = 0.0; visits = 0; parent = None; children = [] } in

  let jouer_adversaire () =
    match adversaire with
    | 1 ->
        if !k <> 0 then root := get_node !e !root;
        e := jouer_coup_avec_mcts !root 5000 1.41;
        root := get_node !e !root;
    | 2 -> e := meilleur_coup_2 !e d Minf Pinf heuristique
    | _ ->
        let c = next_move () in
        e := joue !e c !k
  in

  while (est_gagnant !e = None) && (!k < 81) do
    if !joueur_commence then (
      let c = next_move () in
      e := joue !e c !k;
      k := !k + 1
    );
    
    print_grid !e.grid;
    jouer_adversaire ();
    k := !k + 1;
    print_grid !e.grid;
    joueur_commence := not !joueur_commence; (* Correction ici *)
  done;

  afficher_gagnant !e