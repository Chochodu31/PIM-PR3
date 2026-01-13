with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;            use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;              use Ada.Float_Text_IO;
with Ada.Command_Line;               use Ada.Command_Line;
with Fonctions_globales;             use Fonctions_globales;
with Cache_Arbre;                    use Cache_Arbre;
with SDA_Exceptions;                 use SDA_Exceptions;
with Routeur_exceptions;             use Routeur_exceptions;

procedure Routeur_LA is
   
   -- Variables pour les options de ligne de commande
   Taille_Cache       : Integer;          -- Taille du cache (-c)
   Politique_Cache    : Tab_Politique;    -- Politique de suppression (-p)
   Afficher_Stats     : Boolean;          -- Afficher les statistiques (-s/-S)
   Fichier_Table      : Unbounded_String; -- Fichier de table de routage (-t)
   Fichier_Paquets    : Unbounded_String; -- Fichier de paquets à router (-q)
   Fichier_Resultats  : Unbounded_String; -- Fichier de résultats (-r)
   
   -- Fichiers d'entrée/sortie
   Fichier_Entree     : File_Type;
   Fichier_Sortie     : File_Type;
   
   -- Structures de données
   Table_Routage      : T_LCA;            -- Table de routage complète
   Cache              : T_Cache;          -- Cache des routes
   
   -- Statistiques d'exécution
   Nb_Paquets_Traites : Integer := 0;     -- Nombre total de paquets traités
   Nb_Defauts_Cache   : Integer := 0;     -- Défauts de cache pendant l'exécution
   
   -- Rechercher une interface pour une adresse IP en utilisant le cache
   function Obtenir_Interface_Cachee(Adresse_IP : T_Adresse_IP) return Unbounded_String is
      Interface_Trouvee : Unbounded_String;
      Valeur_Temp       : T_Case;
   begin
      begin
         -- Essaie d'abord de trouver dans le cache
         Interface_Trouvee := Rechercher(Cache, Adresse_IP);
         
         -- Met à jour les métadonnées selon la politique
         case Politique_Cache is
            when LRU =>
               -- Pour LRU, on doit trouver la route correspondante pour mettre à jour son horodatage
               declare
                  Masque_Max : T_Adresse_IP := 0;
               begin
                  for i in 1..Taille(Table_Routage) loop
                     begin
                        Valeur_Temp := La_Valeur(Table_Routage, i);
                        if ((Adresse_IP and Valeur_Temp.Masque) = Valeur_Temp.Destination) 
                           and (Valeur_Temp.Masque >= Masque_Max) then
                           Masque_Max := Valeur_Temp.Masque;
                        end if;
                     exception
                        when Cle_Absente_Error => null;
                     end;
                  end loop;
               end;
               
            when LFU =>
               -- Même approche pour LFU (incrémenter le compteur d'accès)
               declare
                  Masque_Max : T_Adresse_IP := 0;
               begin
                  for i in 1..Taille(Table_Routage) loop
                     begin
                        Valeur_Temp := La_Valeur(Table_Routage, i);
                        if ((Adresse_IP and Valeur_Temp.Masque) = Valeur_Temp.Destination) 
                           and (Valeur_Temp.Masque >= Masque_Max) then
                           Masque_Max := Valeur_Temp.Masque;
                        end if;
                     exception
                        when Cle_Absente_Error => null;
                     end;
                  end loop;
               end;
               
            when others =>
               null;  -- FIFO ne nécessite pas de mise à jour
         end case;
         
         return Interface_Trouvee;
         
      exception
         when Cle_Absente_Error =>
            -- Défaut de cache : route non trouvée dans le cache
            Nb_Defauts_Cache := Nb_Defauts_Cache + 1;
            
            -- Chercher dans la table de routage complète
            Interface_Trouvee := Association_ad_des(Table_Routage, Adresse_IP);
            
            -- Ajouter la route trouvée au cache (avec éviction si nécessaire)
            declare
               Masque_Max     : T_Adresse_IP := 0;
               Destination_Max : T_Adresse_IP;
               Interface_Max  : Unbounded_String;
               Valeur         : T_Case;
            begin
               -- Trouver la route avec le masque le plus long
               for i in 1..Taille(Table_Routage) loop
                  begin
                     Valeur := La_Valeur(Table_Routage, i);
                     if ((Adresse_IP and Valeur.Masque) = Valeur.Destination) 
                        and (Valeur.Masque >= Masque_Max) then
                        Masque_Max := Valeur.Masque;
                        Destination_Max := Valeur.Destination;
                        Interface_Max := Valeur.Int;
                     end if;
                  exception
                     when Cle_Absente_Error => null;
                  end;
               end loop;
               
               -- Si le cache est plein, on supprime une route selon la politique
               if Taille(Cache) >= Cache.Taille_Max then
                  case Politique_Cache is
                     when FIFO => Supprimer_FIFO(Cache);
                     when LRU => Supprimer_LRU(Cache);
                     when LFU => Supprimer_LFU(Cache);
                  end case;
               end if;
               
               -- Ajouter la nouvelle route au cache
               Enregistrer(Cache, Destination_Max, Masque_Max, Interface_Max);
            end;
            
            return Interface_Trouvee;
      end;
   end Obtenir_Interface_Cachee;
   
   -- Traiter les commandes spéciales (table, cache, stat, fin)
   procedure Traiter_Commande(Commande : String; Ligne : Integer) is
      Nb_Defauts, Nb_Demandes : Integer;
      Taux : Float;
   begin
      if Commande = "table" then
         -- Afficher la table de routage complète
         Afficher_table_routage(Table_Routage);
         
      elsif Commande = "cache" then
         -- Afficher le contenu du cache
         Put_Line("=== Contenu du cache ===");
         Afficher(Cache);
         Put_Line("Taille: " & Integer'Image(Taille(Cache)) & "/" & Integer'Image(Cache.Taille_Max));
      elsif Commande = "stat" then
         -- Afficher les statistiques si activées
         if Afficher_Stats then
            Obtenir_Statistiques(Cache, Nb_Defauts, Nb_Demandes, Taux);
            Put_Line("=== Statistiques ===");
            Put_Line("Taille cache: " & Integer'Image(Taille_Cache));
            Put_Line("Politique: " & Tab_Politique'Image(Politique_Cache));
            Put_Line("Demandes: " & Integer'Image(Nb_Demandes));
            Put_Line("Défauts: " & Integer'Image(Nb_Defauts));
            Put("Taux défauts: ");
            Put(Taux, Fore => 1, Aft => 4, Exp => 0);
            New_Line;
            Put_Line("Paquets traités: " & Integer'Image(Nb_Paquets_Traites));
         else
            Put_Line("Statistiques désactivées (option -S)");
         end if;
      elsif Commande = "fin" then
         -- Terminer le traitement
         raise End_Error;
      else
         -- Commande inconnue
         Put("Erreur ligne ");
         Put(Ligne, 1);
         New_Line;
         raise Commande_Inconnu_Error;
      end if;
   end Traiter_Commande;
   
   -- Traiter le fichier de paquets ligne par ligne
   procedure Traiter_Fichier_Paquets is
      Ligne_Courante : Unbounded_String;
      Numero_Ligne   : Integer := 0;
      Adresse_IP     : T_Adresse_IP;
      Interface      : Unbounded_String;
   begin
      while not End_Of_File(Fichier_Entree) loop
         Ligne_Courante := To_Unbounded_String(Get_Line(Fichier_Entree));
         Numero_Ligne := Numero_Ligne + 1;
         Trim(Ligne_Courante, Both);
         
         if Length(Ligne_Courante) > 0 then
            -- Déterminer si c'est une adresse IP ou une commande
            if Element(Ligne_Courante, 1) in '0'..'9' then
               -- Adresse IP : la route et écrire le résultat
               begin
                  Adresse_IP := Id_ad_IP(To_String(Ligne_Courante));
                  Interface := Obtenir_Interface_Cachee(Adresse_IP);
                  
                  -- Écrire dans le fichier de sortie
                  Ecrire_Ad_IP(Fichier_Sortie, Adresse_IP);
                  Put(Fichier_Sortie, " ");
                  Put(Fichier_Sortie, To_String(Interface));
                  New_Line(Fichier_Sortie);
                  
                  Nb_Paquets_Traites := Nb_Paquets_Traites + 1;
                  
               exception
                  when Adresse_IP_Introuvable_Error =>
                     Put_Line("Erreur IP ligne" & Integer'Image(Numero_Ligne));
               end;
            else
               -- traitement d'une Commande spéciale
               Traiter_Commande(To_String(Ligne_Courante), Numero_Ligne);
            end if;
         end if;
      end loop;
   exception
      when End_Error =>
         null;  
   end Traiter_Fichier_Paquets;
   
begin
   -- Lire les options de la ligne de commande
   Gerer_commandes(
      Cache => Taille_Cache,
      Politique => Politique_Cache,
      Statistique => Afficher_Stats,
      Table => Fichier_Table,
      Paquet => Fichier_Paquets,
      Resultat => Fichier_Resultats
   );
   
   -- Initialiser le cache avec la taille spécifiée
   Initialiser(Cache, Taille_Cache);
   
   -- Charger la table de routage depuis le fichier
   Initialiser(Table_Routage);
   Table_routage(To_String(Fichier_Table), Table_Routage);
   
   -- Ouvrir les fichiers d'entrée et de sortie
   Ouvrir(To_String(Fichier_Paquets), Fichier_Entree);
   Create(Fichier_Sortie, Out_File, To_String(Fichier_Resultats));
   
   -- Traiter les paquets
   Traiter_Fichier_Paquets;
   
   -- Afficher les statistiques finales si demandé
   if Afficher_Stats then
      New_Line;
      Put_Line("=== FIN ===");
      Put_Line("Paquets: " & Integer'Image(Nb_Paquets_Traites));
      Put_Line("Défauts cache: " & Integer'Image(Nb_Defauts_Cache));
   end if;
   
   -- Nettoyage des ressources
   Close(Fichier_Entree);
   Close(Fichier_Sortie);
   Detruire(Table_Routage);
   Detruire(Cache);
   
   Put_Line("Terminé. Résultats dans " & To_String(Fichier_Resultats));
   
exception
   when Fichier_Inconnu_Error =>
      Put_Line("Erreur: Fichier introuvable");
      
   when Commande_Inconnu_Error =>
      Put_Line("Erreur: Commande inconnue");
      
   when others =>
      Put_Line("Erreur inattendue");
      
end Routeur_LA;