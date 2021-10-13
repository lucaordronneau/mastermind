#use "code.ml";;
#use "affichage.ml";;
#use "IA.ml";;
module Mastermind : sig 
  
  val mastermind : string -> int -> int -> bool -> unit  
    
  val mastermind2 : string -> string -> int -> int -> unit

  val mastermind3 : int -> int -> unit  
    
end = struct
  
  type joueur = {nom : string; points : int; humain : bool};;
  type joueurs = {un : joueur; deux : joueur};;
  
  (** Création d'un joueur humain + creation d'un joueur Ordinateur
   * @param nom du joueur humain
   * @return un enregistrement de 2 joueurs avec au depart 0 points
   *)
  let creationjoueurs nom_joueur = {un = {nom = nom_joueur; points = 0; humain = true}; deux = {nom = "Ordinateur"; points = 0; humain =false}}
  (** Création de 2 joueurs humains
   * @param nom du 1er joueur humain
   * @param nom du 2eme joueur humain
   * @return un enregistrement de 2 joueurs humains avec au depart 0 points
   *)
                                 
  let creationjoueurs2 nom1 nom2 =  {un = {nom = nom1; points = 0; humain = true}; deux = {nom = nom2; points = 0; humain =true}};;
  (** Création de 3 joueurs humains
   * @return un enregistrement de 2 ordinateurs avec au depart 0 points
   *)
                                 
  let creationjoueurs3 =  {un = {nom = "Ordinateur1"; points = 0; humain = false}; deux = {nom = "Ordinateur2"; points = 0; humain =false}};;

  (** Choisit aléatoirement le joueur qui commence
   * @param couplejoueurs de type joueurs
   * @return couplejoueurs de type joueurs
   *)
  
  let commence couplejoueurs = let b = Random.bool() in
                               match couplejoueurs with
                               | {un = _; deux = _} when b -> couplejoueurs
                               | {un = x; deux = y} -> {un = y; deux = x};;

  (** Renvoie un nombre de parties paires
   * @param un nombre de parties
   * @return un nombre de parties paires si celui en parametre est impaire
   *)
  let nb_parties_pairs n = if (n mod 2 = 0) then n else (n+1);;
  (** Choisit une methode : 0 -> Naïf, 1 -> Knuth, 2 -> Knuth min-min, 3 -> Naïf random
   *)
  
  let methode = 0;;
  
  let rec random_code nb_pion couleur_max acc =
    if nb_pion = 0 then
      acc
    else
      random_code (nb_pion-1) couleur_max (Random.int couleur_max :: acc);;
  
  (** Créée un code random selon le nbe de pions et le nbe de couleurs max
   * @return une liste d'entier
   *)
  let creation_random_code = fun () -> random_code Code.nombre_pions (List.length Code.couleurs_possibles) [];;
  
  (** Créée un code (le code de depart) soit via l'ordi soit via l'humain
   * @param participant de type joueur
   * @return un Code.t
   *)
  let rec saisie_ordi_humain participant = if participant = {nom = "Ordinateur"; points = participant.points; humain =false} or participant = {nom = "Ordinateur1"; points = participant.points; humain =false} or participant = {nom = "Ordinateur2"; points = participant.points; humain =false} then
                                             (print_string ("L'"^participant.nom^" crée le code"); print_newline ();print_newline (); creation_random_code ())
                                           else
                                             let saisie = read_line (print_string ("\027[33m C'est au tour de : "^participant.nom^"\027[37m"); print_newline(); print_newline();Affichage.afficher_couleurs_possibles (List.length Code.couleurs_possibles); print_newline ();print_string "\027[31mCréez\027[37m le code caché en l'écrivant de la forme |rouge|bleu|..| : ") in
                                             let crlscr = Sys.command("clear") in
                                             let code_opt = Code.code_of_string saisie in
                                             match code_opt with
                                             | None -> print_string "Saisie Invalide"; print_newline (); print_newline (); saisie_ordi_humain participant
                                             | Some(x) when List.length x <> Code.nombre_pions -> print_string "Saisie Invalide"; print_newline (); print_newline (); saisie_ordi_humain participant
                                             | Some(x) -> x;;
  
  (** Créée un code saisie par l'humain (utilisé quand l'humain veut tenter un code)
   * @param participant humain
   * @return un Code.t
   *)
  let rec saisie_humain participant = let saisie = read_line (print_string ("\027[33m C'est au tour de : "^participant.nom^"\027[37m"); print_newline(); print_newline() ; Affichage.afficher_couleurs_possibles (List.length Code.couleurs_possibles); print_newline (); print_string "\027[33mTentez\027[37m un code en l'écrivant de la forme |rouge|bleu|..| : " ) in
                                      let crlscr = Sys.command("clear") in
                                      let code_opt = Code.code_of_string saisie in
                                      match code_opt with
                                      | None -> print_string "Saisie Invalide"; print_newline (); print_newline (); saisie_humain participant
                                      | Some(x) when List.length x <> Code.nombre_pions -> print_string "Saisie Invalide"; print_newline (); print_newline (); saisie_ordi_humain participant
                                      | Some(x) -> x;;

  (** Permet à l'IA de tenter des codes, paterne non exhaustif car la fonction reponse ne doit pas pouvoir renvoyer None
   * @param le nombres de tentatives
   * @param le vrai_code
   * @param le participant de type joueur
   * @param la liste des codes possibles (Code.t)
   * @param la liste des codes essayé au départ vide
   * @param reponse au code tenter (int*int) option
   * @return retourne le participant de type joueur avec le nbe de pts changé ou pas
   *)
  let rec run_tentatives_IA tentatives vrai_code participant possibles essaye reponses =
    match tentatives with
    | 0 -> participant
    | e -> let code = (IA.choix methode essaye possibles) in
           let reponse = Code.reponse code vrai_code in
           match reponse with
           | Some(a,b) when a = List.length vrai_code -> Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_string ("\027[31m   +1 point pour "^participant.nom^"\027[37m") ; print_newline ();print_newline (); {nom = participant.nom ; points = participant.points+1 ; humain = participant.humain}
           | Some(a,b) -> let nw_possibles = IA.filtre methode (code,reponse) possibles in
                          Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_newline (); run_tentatives_IA (e-1) vrai_code participant nw_possibles (code :: essaye) (reponse :: reponses);;
  
  (** Permet à l'humain  de tenter des codes, paterne non exhaustif car la fonction reponse ne doit pas pouvoir renvoyer None
   * @param le nombres de tentatives
   * @param le vrai_code
   * @param le participant de type joueur
   * @param la liste des codes essayé au départ vide
   * @param reponse au code tenté (int*int) option
   * @return retourne le participant de type joueur avec le nbe de pts changé ou pas
   *)
  let rec run_tentatives_humain tentatives vrai_code participant essaye reponses =
    match tentatives with
    | 0 -> participant
    | e -> let code = (saisie_humain participant) in
           let reponse = Code.reponse code vrai_code in
           match reponse with
           | Some(a,b) when a = List.length vrai_code -> let crlscr = Sys.command("clear") in
                                                         Affichage.affiche_plusieurs_codes_et_reponses essaye reponses; Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_string "\027[31m   +1 point pour vous \027[37m"; print_newline ();print_newline (); {nom = participant.nom ; points = participant.points+1 ; humain = participant.humain}
           | Some(a,b) -> let crlscr = Sys.command("clear") in
                          Affichage.affiche_plusieurs_codes_et_reponses essaye reponses; Affichage.afficher_code code; Affichage.afficher_reponse (a,b);print_newline ();print_newline (); run_tentatives_humain (e-1) vrai_code participant (code::essaye) ((a,b) :: reponses);;

  (** Permet de génerer un nombre de parties donné tout en alternant IA et humain selon le nbe de parties
   * @param le nombres de parties
   * @param le couple de joueurs de type joueurs
   * @param le nbe de tentatives par parties
   * @return retourne le couplejoueurs de type joueurs
   *)
  let rec run_parties parties couplejoueurs tentatives  =
    match parties with
    | 0 -> couplejoueurs
    | e when (e mod 2 = 0) && couplejoueurs.un.humain -> let participant = run_tentatives_IA tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux Code.tous [[]] [] in
                                                         run_parties (e-1) {un = couplejoueurs.un; deux = participant} tentatives
    | e when (e mod 2 <> 0) && couplejoueurs.deux.humain -> let participant = run_tentatives_IA tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un Code.tous [[]] [] in
                                                            run_parties (e-1) {un = participant; deux = couplejoueurs.deux} tentatives
    | e when (e mod 2 = 0) && (not couplejoueurs.un.humain) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux [[]] [] in
                                                               run_parties (e-1) {un = couplejoueurs.un; deux = participant} tentatives
    | e when (e mod 2 <> 0) && (not couplejoueurs.deux.humain) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un [[]] [] in
                                                                  run_parties (e-1) {un = participant; deux = couplejoueurs.deux} tentatives;;


  (** Permet au joueur humain de rentrer les reponses Bien Placée Mal Placée des tentatives de l'IA
   * @param code entré par l'IA
   * @param vrai_code
   * @return renvoie les reponse si c'est bon sinon arret du programme  
   *)
  let rec reponse_humain code vrai_code = let (a,b) = Affichage.afficher_code vrai_code; print_string " Code proposé / Code à deviner "; print_newline(); (read_int ((print_string "Entrez le nombre de pions bien placés : ")), read_int ((print_string "Entrez le nombre de pions mal placés : "))) in
                                          match Code.reponse code vrai_code with
                                          | None -> (false,None)
                                          | Some(x,y) when (a<>x) || (b<> y) -> print_string "Vous vous êtes trompé ou vous avez essayé de tricher !!! "; print_newline (); (false,Some(x,y)) 
                                          | Some(x,y) when (a = x) && (b = y) -> (true,Some(x,y));;


  (** Permet à l'humain de repondre aux tentatives de l'IA sans que ça le fasse automatiquement, paterne non exhaustif car la fonction reponse ne doit pas pouvoir renvoyer None
   * @param le nombres de tentatives
   * @param le vrai_code
   * @param le participant de type joueur
   * @param la liste des codes possibles (Code.t)
   * @param la liste des codes essayé au départ vide
   * @param reponse au code tenter (int*int) option rentré par l'humain via la fonction reponse_humain
   * @return retourne le participant de type joueur avec le nbe de pts changé ou pas
   *)
  let rec run_reponse_pas_auto tentatives vrai_code participant possibles essaye reponses =
    match tentatives with
    | 0 -> participant
    | e -> let code = (IA.choix methode essaye possibles) in
           Affichage.afficher_code code; print_newline (); let reponse = reponse_humain code vrai_code in
                                                           match reponse with
                                                           | (true,Some(a,b)) when a = List.length vrai_code -> Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_string ("\027[31m   +1 point pour "^participant.nom^" \027[37m") ; print_newline ();print_newline (); {nom = participant.nom ; points = participant.points+1 ; humain = participant.humain}
                                                           | (true,Some(a,b)) -> let nw_possibles = IA.filtre methode (code,Some(a,b)) possibles in
                                                                          run_reponse_pas_auto (e-1) vrai_code participant nw_possibles (code :: essaye) (Some(a,b) :: reponses)
                                                           | (false,Some(a,b)) -> Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_string ("\027[31m   +1 point pour "^participant.nom^"\027[37m") ; print_newline ();print_newline (); {nom = participant.nom ; points = participant.points+1 ; humain = participant.humain};;


  (** Permet de génerer un nombres de parties donné tout en alternant IA et humain selon le nbe de parties et que l'humain controle les les tentatives de l'IA
   * @param le nombres de parties
   * @param le couple de joueurs de type joueurs
   * @param le nbe de tentatives par parties
   * @return retourne le couplejoueurs de type joueurs
   *)
  let rec run_parties_pas_auto parties couplejoueurs tentatives =
    match parties with
    | 0 -> couplejoueurs
    | e when (e mod 2 = 0) && couplejoueurs.un.humain -> let participant = run_reponse_pas_auto tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux Code.tous [[]] [] in
                                                         run_parties_pas_auto (e-1) {un = couplejoueurs.un; deux = participant} tentatives
    | e when (e mod 2 <> 0) && couplejoueurs.deux.humain -> let participant = run_reponse_pas_auto tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un Code.tous [[]] [] in
                                                            run_parties_pas_auto (e-1) {un = participant; deux = couplejoueurs.deux} tentatives
    | e when (e mod 2 = 0) && (not couplejoueurs.un.humain) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux [[]] [] in
                                                               run_parties_pas_auto (e-1) {un = couplejoueurs.un; deux = participant} tentatives
    | e when (e mod 2 <> 0) && (not couplejoueurs.deux.humain) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un [[]] [] in
                                                                  run_parties_pas_auto (e-1) {un = participant; deux = couplejoueurs.deux} tentatives;;



  (** Permet de determiner le gagnant selon le nbe de points des participants
   * @param couplejoueurs de type joueurs
   * @return le gagnant (unit)
   *)
  let gagnant couplejoueurs =
    match couplejoueurs.un.points with
    | points when (points = couplejoueurs.deux.points) -> print_string ("\027[33m Il y a égalité \027[37m")
    | points when (points < couplejoueurs.deux.points) -> print_string ("\027[33m Le gagnant est : "^couplejoueurs.deux.nom^"\027[37m avec \027[33m"^string_of_int couplejoueurs.deux.points^"\027[37m points")
    | points when (points > couplejoueurs.deux.points) -> print_string ("\027[33m Le gagnant est : "^couplejoueurs.un.nom^"\027[37m avec \027[33m"^string_of_int couplejoueurs.un.points^"\027[37m points") ;;



  (** Permet de jouer au mastermind humain vs IA
   * @param le nom du joueur humain
   * @param le nbe de tentatives pour les parties
   * @param le nbe de parties à jouer (tjrs paires)
   * @param boolean permettant de dire si les reponses sont calculé de maniéres auto ou pas
   * @return le gagnant
   *)
  let mastermind nom tentatives parties auto =
    let crlscr = Sys.command("clear") in
    let participants = creationjoueurs nom in
    let couplejoueurs = commence participants in
    let partie_pairs = nb_parties_pairs parties in
    match auto with
    | true -> let winner = run_parties partie_pairs couplejoueurs tentatives in gagnant winner
    | false -> let winner = run_parties_pas_auto partie_pairs couplejoueurs tentatives in gagnant winner;;
  
  (** Permet de génerer un nombres de parties donné tout en alternant le joueur1 et joueur2 selon le nbe de parties
   * @param le nombres de parties
   * @param le couple de joueurs de type joueurs
   * @param le nbe de tentatives par parties
   * @return retourne le couplejoueurs de type joueurs
   *)
  let rec run_parties_pvp parties couplejoueurs tentatives  =
    match parties with
    | 0 -> couplejoueurs
    | e when (e mod 2 = 0) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux [[]] []
                              in run_parties_pvp (e-1) {un = couplejoueurs.un; deux = participant} tentatives
    | e when (e mod 2 <> 0) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un [[]] []
                               in run_parties_pvp (e-1) {un = participant; deux = couplejoueurs.deux} tentatives;;

  (** Permet de jouer au mastermind humain vs humain
   * @param le nom du joueur 1 humain
   * @param le nom du joueur 2 humain
   * @param le nbe de tentatives pour les parties
   * @param le nbe de parties à jouer 
   * @return le gagnant
   *)
  let mastermind2 nom1 nom2 tentatives parties =
    let crlscr = Sys.command("clear") in
    let participants = creationjoueurs2 nom1 nom2 in
    let couplejoueurs = commence participants in
    let partie_pairs = nb_parties_pairs parties in
    let winner = run_parties_pvp partie_pairs couplejoueurs tentatives in
    gagnant winner;;

  (** Permet de génerer un nombres de parties donné tout en alternant l'ordinateur1 et l'ordinateur2 selon le nbe de parties
   * @param le nombres de parties
   * @param le couple de joueurs de type joueurs
   * @param le nbe de tentatives par parties
   * @return retourne le couplejoueurs de type joueurs
   *)
  let rec run_parties_eve parties couplejoueurs tentatives =
    match parties with
    | 0 -> couplejoueurs
    | e when (e mod 2 = 0) -> let participant = run_tentatives_IA tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux Code.tous [[]] [] in
                                                         run_parties_eve (e-1) {un = couplejoueurs.un; deux = participant} tentatives
    | e when (e mod 2 <> 0) -> let participant = run_tentatives_IA tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un Code.tous [[]] [] in
                                                            run_parties_eve (e-1) {un = participant; deux = couplejoueurs.deux} tentatives;;

  (** Permet de jouer au mastermind IA vs IA
   * @param le nbe de tentatives pour les parties
   * @param le nbe de parties à jouer 
   * @return le gagnant
   *)
  let mastermind3 tentatives parties =
    let crlscr = Sys.command("clear") in
    let participants = creationjoueurs3  in
    let couplejoueurs = commence participants in
    let partie_pairs = nb_parties_pairs parties in
    let winner = run_parties_eve partie_pairs couplejoueurs tentatives in
    gagnant winner;;



end;;
open Mastermind;;
