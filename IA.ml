 (** Algorithmes de recherche de code *)
module IA :
sig
  (** Nombre d'algorithmes developpes *)
  val nombre_methodes : int
  (** Choisit un code a proposer
   * @param methode 0 pour l'algorithme naif,
   * 1 pour l'algorithme de KNUTH
   * ... et ainsi de suite
   * @param essais la liste des codes deja proposes
   * @param possibles la liste des codes possibles
   * @return le prochain code a essayer
   *)
  val choix : int -> Code.t list -> Code.t list -> Code.t
  (** Filtre les codes possibles
   * @param methode 0 pour l'algorithme naif,
   * 1 pour l'algorithme de KNUTH
   * ... et ainsi de suite
   * @param (code, rep) le code essaye et la reponse correspondante
   * @param possibles la liste de courante de codes possibles
   * @return la nouvelle liste de codes possibles
   *)
  val filtre : int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list
end = struct
  
  let nombre_methodes = 4;;
  
  (** Compare deux codes
   * @param code1 un code 
   * @param code2 un code 
   * @return vrai si les codes sont égaux, faux sinon
   *)
  let rec code_egaux code1 code2 =
    match (code1,code2) with
    | ([],[]) -> true
    | (a :: ls1,b :: ls2) -> if a = b then
                               code_egaux ls1 ls2  
                             else
                               false
    | _ -> false;;
  
  (** Vérifie si il existe un code dans une liste 
   * @param code un code
   * @param essais la liste courante des codes déjà essayé
   * @return vrai si le code est contenue dans la liste sinon faux
   *)
  let rec existe code essais =
    match essais with
    | [] -> false
    | v :: ls -> if code_egaux code v then
                   true
                 else
                   existe code ls;;
  
  (** Calcule le poids d'un code par rapport à une réponse et la liste des réponses
   * @param code un code
   * @param rep réponse testé
   * @param possibles la liste courante des codes possibles
   * @param acc le nombre de code ayant la même réponse que code
   * @return acc le poids du code pour cette réponse
   *)
  let rec filtre_choix_knuth code rep possibles acc =
    match possibles with
    | [] -> acc
    | v :: ls -> match ((Code.reponse code v),rep) with
                 | (None,_) -> filtre_choix_knuth code rep ls acc
                 | (Some(a,b),(x,y)) -> if (a = x) && (b = y) then
                                          filtre_choix_knuth code rep ls (acc+1)
                                        else
                                          filtre_choix_knuth code rep ls acc;;
  
  (** Calcule le poids d'un code par rapport à la liste des réponses posiible et des codes possibles 
   * @param code le code pour le poids
   * @param possibles la liste courantes des codes possibles
   * @return le poids maximum du code
   *)
  let poids_init code possibles = let deb = filtre_choix_knuth code (List.hd (Code.toutes_reponses)) possibles 0 and fin = List.tl Code.toutes_reponses in
                             List.fold_left (fun acc x -> let y = filtre_choix_knuth code x possibles 0 in if acc > y then acc else y) deb fin;;

  (** Calcule le poids d'un code par rapport à la liste des réponses posiible et des codes possibles 
   * @param code le code pour le poids
   * @param possibles la liste courantes des codes possibles
   * @return le poids maximum du code
   *)
  let rec poids_test_rec pm liste code possibles acc =
    match liste with
    | [] -> acc
    | v :: ls -> (match filtre_choix_knuth code v possibles 0 with
                 | x when x > pm -> x
                 | x when acc < x -> poids_test_rec pm ls code possibles x
                 | _ -> poids_test_rec pm ls code possibles acc);;
             
  (** Calcule le poids d'un code par rapport à la liste des réponses posiible et des codes possibles 
   * @param code le code pour le poids
   * @param possibles la liste courantes des codes possibles
   * @return le poids maximum du code avec un système d'élagage
   *)
  let poids pm code possibles = let acc = filtre_choix_knuth code (List.hd (Code.toutes_reponses)) possibles 0 in
                                     if pm < acc then
                                       acc
                                     else
                                       poids_test_rec pm (List.tl (Code.toutes_reponses)) code possibles acc;;
  
  (** Calcule le poids d'un code par rapport à la liste des réponses posiible et des codes possibles 
   * @param code le code pour le poids
   * @param possibles la liste courantes des codes possibles
   * @return le poids minimum du code
   *)
  let poids_2 code possibles = let deb = filtre_choix_knuth code (List.hd (Code.toutes_reponses)) possibles 0 and fin = List.tl Code.toutes_reponses in
                               List.fold_left (fun acc x -> let y = filtre_choix_knuth code x possibles 0 in if acc < y then acc else y) deb fin;;
  
  (** Choisie le prochain code à essayer
   * @param essais la liste courante des codes déjà essayé
   * @param possibles la liste courante des codes possibles
   * @param liste_tous la liste de tous les codes réalisable
   * @param res le prochain code à essayer
   * @return le prochain code à essayer d'après la méthode de Knuth
   *)
  let rec choix_knuth_rec essais possibles liste_tous res =
    match liste_tous with
    | [] -> snd(res)
    | v :: ls when existe v essais -> choix_knuth_rec essais possibles ls res
    | v :: ls -> let x = poids (fst(res)) v possibles in match x with
                                              | x when x > fst(res) -> choix_knuth_rec essais possibles ls res
                                              | x when x = fst(res) -> if (Code.compare v (snd(res))) < 0 then
                                                                         choix_knuth_rec essais possibles ls (x,v)
                                                                       else
                                                                         choix_knuth_rec essais possibles ls res
                                              | _ -> choix_knuth_rec essais possibles ls (x,v);;

  
  (** Choisie le prochain code à essayer
   * @param essais la liste courante des codes déjà essayé
   * @param possibles la liste courante des codes possibles
   * @param liste_tous la liste de tous les codes réalisable
   * @param res le prochain code à essayer
   * @return le prochain code à essayer d'après la méthode de Knuth min/min
   *)
  let rec choix_knuth_rec_2 essais possibles liste_tous res =
    match liste_tous with
    | [] -> snd(res)
    | v :: ls when existe v essais -> choix_knuth_rec_2 essais possibles ls res
    | v :: ls -> let x = poids_2 v possibles in match x with
                                                | x when x > fst(res) -> choix_knuth_rec_2 essais possibles ls res
                                                | x when x = fst(res) -> if (Code.compare v (snd(res))) < 0 then
                                                                           choix_knuth_rec_2 essais possibles ls (x,v)
                                                                         else
                                                                           choix_knuth_rec_2 essais possibles ls res
                                                | _ -> choix_knuth_rec_2 essais possibles ls (x,v);;
  
  (** Choisie le prochain code à essayer
   * @param essais la liste courante des codes déjà essayé
   * @param possibles la liste courante des codes possibles
   * @param liste_tous la liste de tous les codes réalisable
   * @return le prochain code à essayer d'après la méthode de Knuth
   *)
  let rec choix_knuth methode essais possibles liste_tous =
    match liste_tous with
    | [] -> failwith "choix_knuth"
    | v :: ls when existe v essais -> choix_knuth methode essais possibles ls
    | v :: ls -> if methode = 1 then
                   choix_knuth_rec essais possibles ls ((poids_init v possibles), v)
                 else
                   choix_knuth_rec_2 essais possibles ls ((poids_2 v possibles),v);; 
  
  let choix methode essais possibles =
    match methode with
    | 0 -> List.hd possibles 
    | x when (x = 1) || (x = 2) -> (match possibles with
                                    | [] -> failwith "choix"
                                    | v :: [] -> v
                                    | _ -> choix_knuth methode essais possibles Code.tous)
    | 3 -> List.nth possibles (Random.int (List.length possibles));;
  
  
  (** Supprime un code dans une liste de code
   * @param x un code
   * @param l une liste de code
   * @return la liste de code sans le code x 
   *)
  let rec supprime_un x l =
    match l with
    | [] -> l
    | valeur :: ls -> if code_egaux x valeur then
                        ls
                      else
                        valeur :: supprime_un x ls;;
  
  (** Actualise la liste des codes possibles
   * @param code le dernier code essayé
   * @param rep réponse associé à code
   * @param possibles la liste des codes possibles
   * @return la liste courante des codes possibles
   *)
  let rec filtre_naif code rep possibles =
    match possibles with
    | [] -> possibles
    | v :: ls -> let suivant = filtre_naif code rep ls in
                 match ((Code.reponse code v),rep) with
                 | (None,_) -> suivant
                 | (Some(a,b),Some(x,y)) -> if (a = x) && (b = y) then
                                              v :: suivant
                                            else
                                              suivant;;
  
  let filtre methode der_code possibles =
    match methode with
    | _ -> filtre_naif (fst(der_code)) (snd(der_code)) (supprime_un (fst(der_code)) possibles);;
  
end;;
