(* TYPES *)
(* le type des joueurs *)
type player
(* le type des cellules *)
type cell
(* le type de la grille *)
type grid
(* la grille de morpion classique équivalente *)
type grille_equivalente
(* le type des états du graphe *)
type etat

(* FONCTIONS DES JOUEURS *)
(* retourne le joueur dont c'est le tour *)
val joueur : etat -> player
(* retourne l'autre joueur *)
val autre : player -> player
(* retourne le joueur ayant joué dans la case i j *)
val get_player : grid -> int -> int -> player option

(* FONCTIONS D'UN ÉTAT *)
(* valeur donnant l'état initial *)
val init : int -> int -> etat
(* fonction donnant la grille équivalente à un état *)
val get_case : etat -> int -> int -> player option array

(* FONCTIONS POUR DÉTERMINER LA VICTOIRE *)
(* fonction déterminant si un état terminal est un état de victoire *)
val est_gagnant : etat -> player option
(* retourne le joueur gagnant la grande case d'indice i j none sinon*)
val case_gagnee : grid -> int -> int -> player option

(* FONCTIONS POUR JOUER *)
(* renvoie l'état après jeu dans la case i j *)
val joue : etat -> int * int -> int -> etat
(* fonction calculant les prochains états possibles *)
val possible_moves : etat -> etat list
(* donne les coordonnees de la case du dernier coup joué *)
val trouve_dernier_coup : grid -> int * int

(* MINMAX *)
type int_barre
(* Fonction pour évaluer une sous-grille *)
val evaluation_grille : grid -> int -> int -> player -> int
(* Fonction pour évaluer le plateau *)
val heuristique : player -> etat -> int

(* retourne le max d'une liste *)
val max_list : 'a list -> 'a
(* retourne le min d'une liste *)
val min_list : 'a list -> 'a

(* associe une valeur à un état selon l'algo *)
val minmax : etat -> player -> int -> int_barre
(* renvoie l'état correspondant au jeu après le
    meilleur coup pour le joueur dont c'était le tour *)
val meilleur_coup : etat -> int -> etat
(* élagage alpha beta *)
val minmax_elagage : etat -> player -> int -> int_barre -> int_barre -> int_barre
