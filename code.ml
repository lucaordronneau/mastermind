(** Module de definition d'un code dans le jeu Mastermind *)
module Code :
sig
  (** Le type d'un pion *)
  type pion = int
  (** Le type d'un code *)
  type t = pion list
  (** Nombre de pions par code *)
  val nombre_pions : int
  (** Liste des couleurs possibles *)
  val couleurs_possibles : pion list
  (** Compare deux codes
   * @param code1 premier code a comparer
   * @param code2 second code a comparer
   * @return 0 si les deux codes sont identiques,
   un entier positif si [code1] est strictement plus grand que [code2]
   un entier negatif si [code1] est strictement plus petit que [code2]
   *)
  val compare : t -> t -> int
  (** Conversion code vers chaine de caracteres (pour affichage)
   * @param code code a convertir
   * @return la representation en chaine de caracteres de [code]
   *)
  val string_of_code : t -> string
  (** Conversion chaine de caracteres vers code (pour saisie)
   * @param string chaine de caractere saisie
   * @return le code correspondant a la saisie si la conversion est possible
   [None] si la conversion n'est pas possible
   *)
  val code_of_string : string -> t option
  (** La liste de tous les codes permis *)
  val tous : t list;;
  (** La liste de toutes les reponses possibles *)
  val toutes_reponses : (int * int) list 
  (** Calcule la reponse d'un code par rapport au code cache
   * @param code le code propose
   * @param vrai_code le code cache
   * @return un couple (nombre de pions bien places, nombre de pions mal places)
   [None] si la reponse ne peut etre calculee
   *)
  val reponse : t -> t -> (int * int) option
end = struct 
  type pion = int;;
  type t = pion list;;
  
 (** demande à l'utilisateur le nombre de pions pour jouer
   * @return le nombre de pions par code possibles supérieur à 1
   si l'utilisateur ne rentre pas un nombre supérieur à 1, il lui est demandé de rerentrer son nombre
   *)
  let rec demande_utilisateur_pion = fun () -> let v = read_int (print_string "Entrez le nombre de pion possibles par code (supérieur à 0) :";print_newline ()) in
                                          if v >= 1  then
                                            v 
                                          else
                                            demande_utilisateur_pion () ;;

  let nombre_pions = let crlscr = Sys.command("clear") in 
                     demande_utilisateur_pion ();;
  
  (** Crée une liste de 0 à (n-1)
   * @param nb la longueur de la liste voulue
   * @param acc la liste créé
   * @return la liste de 0 jusqu'à (nb-1) ou la liste vide si nb<0
   *)
  let rec zero_jusque_rec nb acc =
    match (nb-1) with
    | x when x < 0 -> acc
    | 0 -> 0 :: acc
    | x when x > 0 -> zero_jusque_rec x (x :: acc);;
  
  (** Crée une liste de 0 à (n-1)
   * @param nb la longueur de la liste voulue
   * @return la liste de 0 jusqu'à (nb-1) ou la liste vide si nb<0
   *)
  let zero_jusque nb = zero_jusque_rec nb [];;
  
  (** demande à l'utilisateur le nombre de couleurs possibles
   * @return le nombre de couleurs possibles entre 1 et 7,
   si l'utilisateur ne rentre pas un nombre entre 1 et 7 il lui est demandé de rerentrer son nombre
   *)
  let rec demande_utilisateur_couleur = fun () -> let v = read_int (print_string "Entrez un nombre de couleurs possibles entre 1 et 7 :";print_newline ()) in
                                          if v >= 1 && v <=7 then
                                            v 
                                          else
                                            demande_utilisateur_couleur () ;;
  
  let couleurs_possibles = let crlscr = Sys.command("clear") in 
                           zero_jusque (demande_utilisateur_couleur () );;
  
  (** Compare deux codes
   * @param code1 un code
   * @param code2 un code
   * @return la différence de la somme de chaque composante des codes 
   *)
  let rec compare_rec code1 code2 sm1 sm2 =
    match (code1,code2) with
    | ([],[]) -> sm1 - sm2
    | (a :: ls1,b :: ls2) -> compare_rec ls1 ls2 (a+sm1) (b+sm2);;
  
  let rec compare code1 code2 = 
    match (code1,code2) with
    | ([],[]) -> 0
    | (a :: ls1,b :: ls2) -> if a = b then
                               compare ls1 ls2
                             else
                               compare_rec ls1 ls2 0 0;;
  
  let rec string_of_code code = 
      match code with (*paterne non exhaustif car la variable est comprise entre 0 et 7*)
      | [] -> ""
      | 0 :: liste ->  "\027[31m ● "^string_of_code liste
      | 1 :: liste ->  "\027[32m ● "^string_of_code liste
      | 2 :: liste ->  "\027[34m ● "^string_of_code liste
      | 3 :: liste ->  "\027[33m ● "^string_of_code liste
      | 4 :: liste ->  "\027[35m ● "^string_of_code liste
      | 5 :: liste ->  "\027[37m ● "^string_of_code liste
      | 6 :: liste ->  "\027[36m ● "^string_of_code liste;;
  
  (** Transforme une couleur sous forme de string en numéro
   * @param la chaine de caractère décrivant une couleur
   * @return le numéro associé à la couleur en option,
   donc [None] si la couleur en entrée est mal écrite ou qu'elle nexiste pas
   *)
  let num_of_couleur_opt s = 
    match s with
    | "rouge" -> Some(0) 
    | "vert" -> Some(1) 
    | "bleu" -> Some(2) 
    | "jaune" -> Some(3) 
    | "violet" -> Some(4) 
    | "blanc" -> Some(5) 
    | "cyan" -> Some(6) 
    | _ -> None;;
  
  let rec code_of_string s =
    if s = "|" then
      Some([])
    else
      let i_deuxieme_separateur = String.index_from s 1 '|' in
      let s_prem_couleur = String.sub s 1 (i_deuxieme_separateur -1) in
      let suite_s = String.sub s i_deuxieme_separateur (String.rindex s '|' -i_deuxieme_separateur +1) in
      let pion = num_of_couleur_opt s_prem_couleur in
      let apl_rec = code_of_string suite_s in
      match (pion,apl_rec) with
      | (Some(a),Some(b)) -> Some(a :: b)
      | _ -> None;;
  
  (** Créé le code de longueur nb_pion composé que de zéro
   * @param nb_pion le nombre de pion pour un code
   * @param acc le code créé
   * @return le code composé de zéro
   *)
  let rec code_initial nb_pion acc=
    match nb_pion with
    | 0 -> acc
    | _ -> code_initial (nb_pion-1) (0 :: acc);;
  
  (** Crée un nouveau code
   * @param couleur_max entier associé à la plus grande couleur
   * @param code code précédent permettant de créer le nouveau
   * @return le nouveau code créé
   *)
  let rec nouveau_code couleur_max code = 
    match code with
    | [] ->  (false,[])
    | x :: ls when x = couleur_max -> let (a,b) = nouveau_code couleur_max ls in
                                      (a,(0 :: b))
    | x :: ls -> (true,(x+1 :: ls));;
  
  (** Crée tous les codes possibles
   * @param couleur_max entier associé à la plus grande couleur
   * @param acc la liste de tous les codes
   * @return la liste de tous les codes
   *)
  let rec creation_de_tout_code couleur_max acc =
    let (a,b) = (nouveau_code couleur_max (List.hd acc)) in
    if a then
      creation_de_tout_code couleur_max (b :: acc)
    else
      acc;;
  
  (** renvoie le dernier élément d'une liste
   * @param liste la liste
   * @return le dernier élément de la liste
   *)
  let rec fin_liste liste =
    match liste with
    | [] -> failwith "fin_liste"
    | v :: [] -> v
    | v :: ls -> fin_liste ls;;
  
  let tous = let code = code_initial nombre_pions [] in
             creation_de_tout_code (fin_liste couleurs_possibles) ([code]);; 
  
  (** Crée une nouvelle réponse possible
   * @param nb_pion nombre de pion par code
   * @param reponse la réponse précédente
   * @return la nouvelle réponse
   *)
  let rec nouvel_reponse nb_pion reponse =
    match reponse with
    | (a,b) when a = nb_pion -> (false,reponse)
    | (a,b) when (a+b) = nb_pion -> (true,((a+1),0))
    | (a,b) -> (true,(a,(b+1)));;
  
  (** Crée toutes les réponses possibles
   * @param nb_pion nombre de pion par code
   * @param acc la liste de toutes les réponses
   * @return la liste de toutes les réponses possibles
   *)
  let rec creation_de_toutes_reponses nb_pion acc =
    let (a,b) = (nouvel_reponse nb_pion (List.hd acc)) in
    if a then
      creation_de_toutes_reponses nb_pion (b :: acc)
    else
      acc;;
  
  let toutes_reponses = creation_de_toutes_reponses nombre_pions [(0,0)];;
  
  (** Calcule le nombre de pion bien placé
   * @param code1 un code
   * @param code2 un code
   * @return un triplet (nombre de pion bien placé, code1 sans les pions bien placé, code2 sans les pions bien placé)
   *)
  let rec nb_bonne_rep code1 code2 =
    match (code1,code2) with
    | ([],[]) -> (0,[],[])
    | (a :: ls1,b :: ls2) -> let (x,y,z) = nb_bonne_rep ls1 ls2 in
                             if a = b then
                               (x+1,y,z)
                             else
                               (x,a::y,b::z);;
  
  (** supprime un element d'une liste
   * @param x l'élément à supprimer
   * @param l une liste 
   * @return la liste avec un élément x en moins
   *)
  let rec supprime_un x l =
    match l with
    | [] -> (0,l)
    | valeur :: l -> if valeur = x then
                       (1,l)
                     else
                       let res = supprime_un x l in
                       (fst(res),valeur :: snd(res));;
  
  (** Calcul le nombre de mauvaise réponse
   * @param code1 un code
   * @param code2 un code
   * @param acc le nombre de pion mal placé
   * @return le nombre de pion mal placé
   *)
  let rec nb_mauvaise_rep code1 code2 acc =
    match code1 with
    | [] -> acc
    | a :: ls -> let res = supprime_un a code2 in
                 nb_mauvaise_rep ls (snd(res)) (acc+fst(res));;
  
  (** Vérifie si le code rentré est valide
   * @param code le code à vérifier
   * @param nb_vrai_code le nombre de pion dans le vrai code
   * @param couleur_max l'entier maximum possibles dans un code
   * @return true si le code est valide, false sinon
   *)
  let rec code_valide code nb_vrai_code couleur_max =
    match code with
    | [] -> nb_vrai_code = 0 
    | v :: ls -> if v <= couleur_max then code_valide ls (nb_vrai_code-1) couleur_max else false;;
  
  let reponse code vrai_code = if code_valide code (List.length vrai_code) ((List.length couleurs_possibles)-1) then
                                 let (x,y,z) = nb_bonne_rep code vrai_code in
                                 Some(x,nb_mauvaise_rep y z 0)
                               else
                                 None;;
end;;
