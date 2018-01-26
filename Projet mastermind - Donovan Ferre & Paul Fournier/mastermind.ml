(* initialisation *)

let colors = ["rouge";"noir";"vert";"bleu";"jaune";"blanc";"orange";"violet"];;


(* cree une liste de liste avec les deux listes en combinant les éléments de l'une avec l'autre *)  

let rec creer liste liste_suite =
  match liste_suite with 
    |h::t->(h::liste)::(creer liste t)
    |[]->[];;



(* cree une liste de listes en combinants les éléments de la liste 'liste' avec les listes de 'a_combine' *)

let rec combine a_combine liste =
  match a_combine with
    |(h::t)->(creer h liste)@(combine t liste)
    |[]->[];;


(*cree toutes les combinaisons de taille 'taille' composes des elements de la liste 'liste' (avec doublon) *) 

let rec creer_combi liste taille =
  if (taille=1) then (creer [] liste)
  else (combine (creer_combi liste (taille-1)) liste);;


(* savoir s il y a des doublons *)

let rec des_doublons liste=
  match liste with
      []->false
    |h::t->if  List.mem h t then true 
        else (des_doublons t);;


(* appeler la methode supprimer doublons *)

let rec suppr_doublon liste=
  match liste with
      []->[]
    |h::t-> if des_doublons h then
          suppr_doublon t
        else
          h::suppr_doublon t;;



(* configuration de la generation des solutions *)

let configuration_parti taille version_2=
  if taille<1 then 
    failwith"la taille doit etre strictement superieur a zero"
  else 
  if version_2=true then
    creer_combi colors taille
  else
    suppr_doublon(creer_combi colors taille);;


(* test si la liste combi_propose contient la valeur 'valeur' sans que celle ci soit un cas où la case est bien placé d'où la nécessité de vérifier avec la liste
   combi_test que lorsqu'on a trouvé l'élément on a pas retrouvé la valeur dans la liste combi_test à la même place *) 

let rec contient combi_propose combi_test valeur =
  match (combi_propose,combi_test) with
    |([],[])->false
    |(h1::t1,h2::t2)->if (h1=h2) then (contient t1 t2 valeur)
        else
        if(h1=valeur) then true
        else (contient t1 t2 valeur);;

(*modif fait la même chose que contient mais au lieu de renvoyer un booléen elle remplace la valeur par "non" pour éviter de compter plusieurs fois une valeur déjà
  prise en compte dans un traitement précédent enfaite cette méthode est toujours appellé en réponse à la méthode contient lorsqu'elle renvoie 'true' *)

let rec modif combi_propose combi_test valeur  =
  match (combi_propose,combi_test) with
    |(h1::t1,h2::t2)->if(h1=h2) then h1::(modif t1 t2 valeur)
        else
        if (h1 = valeur) then "non"::t1
        else h1::(modif t1 t2 valeur)
    |([],[])->[];;




(*test si une combi 'combi_test' est possible en fonction des paramètres passé 'combi_propose' est la combinaison proposé par l'ordinateur , 'nb_bienplace' est le 
  nombre de pion bien placé dans la 'combi_propose'cette valeur est donné par l'utilisateur , 'nb_malplace' est le nombre de pion mal placé mais de la bonne couleur 
  dans la 'combi_propose' par l'ordinateur cette valeur est donné par l'utilisateur , 'nb_diff' est le nombre de pions qui ne devrait pas se trouvé dans la 
  'combi_proposé' cette valeur est déduite des deux valeurs précédemment données par l'utilisateur *)

let rec test_possible combi_propose combi_test nb_bienplace nb_malplace nb_diff =
  match (combi_propose,combi_test) with
    |(h1::t1,h2::t2)-> if (h2="non") then
          if(h1="non") then (test_possible t1 t2 nb_bienplace nb_malplace nb_diff)
          else 
          if(contient t2 t1 h1) then
            if (nb_malplace=0) then false
            else (test_possible t1 (modif t2 t1 h1) nb_bienplace (nb_malplace-1) nb_diff)
          else(test_possible t1 t2 nb_bienplace nb_malplace nb_diff)
        else
        if (h1=h2) then 
          if (nb_bienplace=0) then false
          else (test_possible t1 t2 (nb_bienplace-1) nb_malplace nb_diff)
        else 
        if (contient t1 t2 h2) then

          if((contient t2 t1 h1)) then
            if (h1="non") then
              if((nb_malplace=0)||(nb_diff=0)) then false
              else (test_possible (modif t1 t2 h2) t2 nb_bienplace (nb_malplace-1) (nb_diff-1))
            else 
            if (nb_malplace<2) then false
            else (test_possible (modif t1 t2 h2) (modif t2 t1 h1) nb_bienplace (nb_malplace-2) nb_diff)
          else
          if (nb_malplace=0) then false
          else (test_possible (modif t1 t2 h2) t2 nb_bienplace (nb_malplace-1) nb_diff)
        else
        if (contient t2 t1 h1) then
          	    if (h1!="non") then
            	      if ((nb_malplace= 0)||(nb_diff=0)) then false
            	      else (test_possible t1 (modif t2 t1 h1) nb_bienplace (nb_malplace-1) (nb_diff-1))
          	    else
          	      if (nb_diff=0) then false
          	      else (test_possible t1 t2 nb_bienplace nb_malplace (nb_diff-1))
        	  else 
        	     if (nb_diff=0) then false
        else (test_possible t1 t2 nb_bienplace nb_malplace (nb_diff-1))
    |([],[])->true;;


(* cette méthode retire de la liste de combinaisons restantes 'liste_combi' tout les combinaisons qui ne sons pas possible par appelle de la fonction test_possible 
   définit ci-dessus*)					 

let rec supprimer combi_propose liste_combi nb_bienplace nb_malplace nb_diff taille =
  if ((nb_bienplace+nb_malplace+nb_diff)!=  taille) then failwith "tricheur !"
  else 
    match liste_combi with
      |h::t->if (test_possible combi_propose h nb_bienplace nb_malplace nb_diff) then h::(supprimer combi_propose t nb_bienplace nb_malplace nb_diff taille)
          else (supprimer combi_propose t nb_bienplace nb_malplace nb_diff taille)
      |[]->[];;


(* donne la premiere combinaison de la liste *)

let premiere_descombinaisons liste=
  match liste with
      []->failwith"la liste est vide"
    |[h]->if h=[] then failwith"la liste est vide" 
        else h
    |h::t->h;;



(* deroulement de la parti  pour un nombre de coups limité par l ordi *)


let rec string_of_list liste =
  match liste with
    |(h::[])-> ""^h^" \n"
    |(h::t)-> h^" ; "^(string_of_list t)
    |[]-> " \n";;

(* deroulement de la parti suite *)

let rec parti ordi_coups liste taille ordi=
  if ordi_coups =0 then failwith " l ordinateur a perdu"
  else 
    match liste with
        []->failwith "tricheur !"
      |[a]->print_string"la combinaison est : \n";
          print_string(string_of_list a);
      |_->print_string"voici la combinaison de l ordinateur : \n";
          print_string  (string_of_list (premiere_descombinaisons liste));
          print_string"combien de pions sont bien place ? \n";
          let pbp=read_int() in
            print_string"combien de pions sont mal place ? \n";
            let pmp=read_int() in 
              if ordi=false then
                parti 1 (supprimer (premiere_descombinaisons liste) liste pbp pmp (taille-(pbp+pmp)) taille) taille false
              else 
                parti (ordi_coups-1) (supprimer (premiere_descombinaisons liste) liste pbp pmp (taille-(pbp+pmp)) taille) taille true;;


(* configurer la parti *)

let jouer=
  print_string("voulez vous jouer avec les doublons ?  oui: 1  non: 0 \n");
  let version=read_int() in
    print_string("quel est la taille de la combinaison ? \n");
    let taille=read_int() in
      print_string("voulez-vous que l ordi ai un nombre limite de coups ? oui: 1 non:0 \n");
      let ordi=read_int() in 
        match (version,ordi) with
            1,0->parti 1 (configuration_parti taille true) taille false
          |0,0->parti 1 (configuration_parti taille false) taille false 
          |1,1->begin print_string("combient de coups a t il droit \n");
              let ordi_coups=read_int() in
                parti ordi_coups (configuration_parti taille true) taille true
            end
          |0,1-> begin print_string("combient de coups a t il droit \n");
              let ordi_coups=read_int() in
                parti ordi_coups (configuration_parti taille false) taille true
            end
          |_,_->failwith"erreure de saisie";;

(* fin du code *)
(* lancement du programme*)
jouer ;;














































				   













