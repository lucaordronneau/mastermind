module Affichage : sig
  
  (** Afffiche un code avec des séparateurs au début et à la fin
   * @param code à écrire dans le terminal
   *)
  val afficher_code : Code.t -> unit
    
  (** Afffiche une réponse dans le terminal
   * @param réponse à écrire dans le terminal
   *)
  val afficher_reponse : (int * int) -> unit
    
  (** Afffiche un code et sa réponse associée dans le terminal à l'aide des fonctions précédentes
   * @param la liste de codes à écrire dans le terminal
   * @param la liste de réponses associées aux codes dans la liste précedente
   *)
  val affiche_plusieurs_codes_et_reponses : Code.t list -> (int * int) list -> unit
    
  (** Afffiche les couleurs possibles pour l'utilisateur
   * @param la taille de la liste de couleurs
   *)
  val afficher_couleurs_possibles : int -> unit 
    
end = struct
  
  let afficher_code code = print_string ("\027[37m |"^Code.string_of_code code^"\027[37m|");;
  
  let afficher_reponse rep =
	match rep with
	| (a,b) -> print_string ("     BP : "^string_of_int a ^"     MP : "^string_of_int b);;
  
  
  let afficher_couleurs_possibles taille_liste_couleurs = 
    match taille_liste_couleurs with (*paterne non exhaustif car la variable est comprise entre 0 et 7*)
    | 0 -> print_string "aucunes couleurs"
    | 1 -> print_string "La seule couleur possible est : rouge"
    | 2 -> print_string "Les couleurs possibles sont : rouge, vert"
    | 3 -> print_string "Les couleurs possibles sont : rouge, vert, bleu"
    | 4 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune"
    | 5 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune, violet"
    | 6 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune, violet, blanc"
    | 7 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune, violet, blanc, cyan";;
  
  
  let rec affiche_plusieurs_codes_et_reponses liste_codes liste_reponses = 
    match (liste_codes,liste_reponses) with (*paterne non exhaustif car ici les listes sont forcément de la même taille*)
    | ([[]],[]) -> print_string ""
    | (v1 :: liste1,v2 :: liste2) -> let res = affiche_plusieurs_codes_et_reponses liste1 liste2 in afficher_code v1;afficher_reponse v2;print_newline ();;
  
end;;
