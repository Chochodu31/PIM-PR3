with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;       use Ada.Float_Text_IO;

with Fonctions_globales;      use Fonctions_globales;
with Cache_Arbre;             use Cache_Arbre;
with SDA_Exceptions;          use SDA_Exceptions;
with Routeur_exceptions;      use Routeur_exceptions;

use Fonctions_globales.LCA_routeur_simple;  -- Pour rendre T_LCA visible

procedure routeur_la is
   
   -- Variables pour les paramètres de ligne de commande
   Taille_Cache : Integer;
   Politique    : Tab_Politique;
   Statistique  : Boolean;
   Table        : Unbounded_String;
   Paquet       : Unbounded_String;
   Resultat     : Unbounded_String;
   
   -- Structures de données
   Tab_Routage  : T_LCA;
   Cache        : T_Cache;
   
   -- Fichiers
   Fichier_Entree  : File_Type;
   Fichier_Sortie  : File_Type;
   
   -- Compteur de lignes
   Ligne_Num : Integer := 0;
   
   -- Fonction pour convertir la politique en chaîne
   function Politique_To_String(P : Tab_Politique) return String is
   begin
      case P is
         when FIFO => return "FIFO";
         when LRU => return "LRU";
         when LFU => return "LFU";
      end case;
   end Politique_To_String;
   
   -- Fonction pour trouver la route correspondante à une adresse IP dans la table
   -- Retourne la route complète (Destination, Masque, Interface)
   function Trouver_Route_Complete(Tab_Routage : T_LCA; Adresse_IP : T_Adresse_IP) return T_Case is
      Meilleure_Route : T_Case;
      Meilleur_Masque : T_Adresse_IP := 0;
      Trouvee         : Boolean := False;
      Route_Courante  : T_Case;
   begin
      for I in 1..Taille(Tab_Routage) loop
         begin
            Route_Courante := La_Valeur(Tab_Routage, I);
            
            -- Vérifier si cette route correspond à l'adresse IP
            if (Adresse_IP and Route_Courante.Masque) = Route_Courante.Destination then
               -- Vérifier si c'est le masque le plus long (préfixe le plus spécifique)
               if Route_Courante.Masque >= Meilleur_Masque then
                  Meilleur_Masque := Route_Courante.Masque;
                  Meilleure_Route := Route_Courante;
                  Trouvee := True;
               end if;
            end if;
         exception
            when Cle_Absente_Error =>
               null; -- Ignorer et continuer
         end;
      end loop;
      
      if not Trouvee then
         raise Adresse_IP_Introuvable_Error;
      end if;
      
      return Meilleure_Route;
   end Trouver_Route_Complete;
   
   -- Traiter une ligne du fichier d'entrée
   -- Retourne True si on doit arrêter le traitement (commande "fin")
   function Traiter_Ligne(Ligne : Unbounded_String) return Boolean is
      Texte_Traite : Unbounded_String := Ligne;
      Adresse_IP   : T_Adresse_IP;
      Interface_Str : Unbounded_String;
      Route_Trouvee : T_Case;
      Arret_Traitement : Boolean := False;
   begin
      Trim(Texte_Traite, Ada.Strings.Both); -- Supprimer les espaces
      
      -- Si la ligne est vide, ignorer
      if Length(Texte_Traite) = 0 then
         Arret_Traitement := False; -- Ne rien faire, continuer
      else
         -- Incrémenter le compteur de lignes
         Ligne_Num := Ligne_Num + 1;
         
         -- Vérifier si c'est une commande ou une adresse IP
         declare
            Texte_Str : constant String := To_String(Texte_Traite);
         begin
            if Texte_Str'Length > 0 and then Texte_Str(1) in '0'..'9' then
               -- C'est une adresse IP
               begin
                  -- Convertir l'adresse IP
                  Adresse_IP := Id_ad_IP(Texte_Str);
                  
                  -- Essayer de trouver dans le cache d'abord
                  begin
                     Interface_Str := Rechercher(Cache, Adresse_IP);
                     
                     -- Si trouvé dans le cache, mettre à jour selon la politique
                     if Politique = LRU then
                        -- Pour LRU, on a besoin de la route exacte pour la mise à jour
                        Route_Trouvee := Trouver_Route_Complete(Tab_Routage, Adresse_IP);
                        Mettre_A_Jour_LRU(Cache, Route_Trouvee.Destination, Route_Trouvee.Masque);
                     elsif Politique = LFU then
                        Route_Trouvee := Trouver_Route_Complete(Tab_Routage, Adresse_IP);
                        Mettre_A_Jour_LFU(Cache, Route_Trouvee.Destination, Route_Trouvee.Masque);
                     end if;
                     
                     -- Écrire le résultat
                     Ecrire(Fichier_Sortie, Adresse_IP, To_String(Interface_Str));
                     
                  exception
                     when Cle_Absente_Error =>
                        -- Non trouvé dans le cache, chercher dans la table de routage
                        Route_Trouvee := Trouver_Route_Complete(Tab_Routage, Adresse_IP);
                        
                        -- Ajouter au cache
                        Enregistrer(Cache, Route_Trouvee.Destination, Route_Trouvee.Masque, Route_Trouvee.Int);
                        
                        -- Écrire le résultat
                        Ecrire(Fichier_Sortie, Adresse_IP, To_String(Route_Trouvee.Int));
                  end;
                  
               exception
                  when Adresse_IP_Introuvable_Error =>
                     Put_Line("Erreur ligne " & Integer'Image(Ligne_Num) & 
                             " : Adresse IP invalide - " & Texte_Str);
                     Arret_Traitement := False;
                  when others =>
                     Put_Line("Erreur ligne " & Integer'Image(Ligne_Num) & 
                             " : Erreur inattendue - " & Texte_Str);
                     Arret_Traitement := False;
               end;
               
            else
               -- C'est une commande
               if Texte_Str = "table" then
                  Afficher_table_routage(Tab_Routage);
                  Arret_Traitement := False;
                  
               elsif Texte_Str = "cache" then
                  Put_Line("=== Contenu du cache ===");
                  Afficher(Cache);
                  Put_Line("Taille: " & Integer'Image(Taille(Cache)) & 
                          "/" & Integer'Image(Taille_Cache));
                  Arret_Traitement := False;
                  
               elsif Texte_Str = "stat" then
                  if Statistique then
                     declare
                        Nb_Defauts, Nb_Demandes : Integer;
                        Taux_Defauts : Float;
                     begin
                        Obtenir_Statistiques(Cache, Nb_Defauts, Nb_Demandes, Taux_Defauts);
                        Put_Line("=== Statistiques du cache ===");
                        Put("Demandes: "); Put(Nb_Demandes, 1); New_Line;
                        Put("Défauts:  "); Put(Nb_Defauts, 1); New_Line;
                        Put("Taux défauts: "); Put(Taux_Defauts, Fore => 1, Aft => 4, Exp => 0); New_Line;
                     end;
                  else
                     Put_Line("Statistiques non activées");
                  end if;
                  Arret_Traitement := False;
                  
               elsif Texte_Str = "fin" then
                  -- Commande "fin" détectée, arrêter le traitement
                  Put_Line("Commande 'fin' détectée à la ligne " & Integer'Image(Ligne_Num));
                  Arret_Traitement := True;
                  
               else
                  Put_Line("Erreur ligne " & Integer'Image(Ligne_Num) & 
                          " : Commande inconnue - " & Texte_Str);
                  Arret_Traitement := False;
               end if;
            end if;
         end;
      end if;
      
      return Arret_Traitement;
   end Traiter_Ligne;
   
   -- Flag pour indiquer que le traitement doit s'arrêter
   Fin_Demandee : Boolean := False;
   
begin
   -- Étape 1: Analyser la ligne de commande
   Gerer_commandes(Taille_Cache, Politique, Statistique, Table, Paquet, Resultat);
   
   Put_Line("=== Routeur avec Cache ===");
   Put_Line("Taille cache: " & Integer'Image(Taille_Cache));
   Put_Line("Politique: " & Politique_To_String(Politique));
   Put_Line("Table routage: " & To_String(Table));
   Put_Line("Fichier paquets: " & To_String(Paquet));
   Put_Line("Fichier résultats: " & To_String(Resultat));
   New_Line;
   
   -- Étape 2: Charger la table de routage
   Initialiser(Tab_Routage);
   Table_routage(To_String(Table), Tab_Routage);
   Put_Line("Table de routage chargée (" & Integer'Image(Taille(Tab_Routage)) & " routes)");
   
   -- Étape 3: Initialiser le cache
   Cache_Arbre.Initialiser(Cache, Taille_Cache);
   Put_Line("Cache initialisé (taille max: " & Integer'Image(Taille_Cache) & ")");
   New_Line;
   
   -- Étape 4: Ouvrir les fichiers
   Ouvrir(To_String(Paquet), Fichier_Entree);
   Create(Fichier_Sortie, Out_File, To_String(Resultat));
   
   -- Étape 5: Traiter les paquets
   Put_Line("Traitement des paquets...");
   while not Fin_Demandee and not End_Of_File(Fichier_Entree) loop
      declare
         Ligne : constant Unbounded_String := 
            To_Unbounded_String(Get_Line(Fichier_Entree));
         Ligne_Fin : Boolean;
      begin
         Ligne_Fin := Traiter_Ligne(Ligne);
         if Ligne_Fin then
            Fin_Demandee := True; -- Arrêter le traitement
         end if;
      end;
   end loop;
   
   if Fin_Demandee then
      Put_Line("Arrêt du traitement suite à la commande 'fin'.");
   else
      Put_Line("Fin du fichier atteinte.");
   end if;
   
   -- Étape 6: Fermer les fichiers
   Close(Fichier_Entree);
   Close(Fichier_Sortie);
   
   -- Étape 7: Afficher les statistiques finales
   if Statistique then
      New_Line;
      Put_Line("=== Statistiques finales ===");
      declare
         Nb_Defauts, Nb_Demandes : Integer;
         Taux_Defauts : Float;
      begin
         Obtenir_Statistiques(Cache, Nb_Defauts, Nb_Demandes, Taux_Defauts);
         Put_Line("Total demandes: " & Integer'Image(Nb_Demandes));
         Put_Line("Total défauts:  " & Integer'Image(Nb_Defauts));
         Put_Line("Taux défauts:   " & Float'Image(Taux_Defauts));
      end;
   end if;
   
   -- Étape 8: Nettoyage
   Detruire(Tab_Routage);
   Detruire(Cache);
   
   Put_Line("=== Traitement terminé ===");
   
exception
   when Fichier_Inconnu_Error =>
      Put_Line("Erreur: Fichier non trouvé.");
   when Commande_Inconnu_Error =>
      Put_Line("Erreur: Commande invalide dans les arguments.");
   when others =>
      Put_Line("Erreur inattendue.");
end routeur_la;