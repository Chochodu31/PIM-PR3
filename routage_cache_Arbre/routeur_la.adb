with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;       use Ada.Float_Text_IO;

with Fonctions_globales;      use Fonctions_globales;
with Cache_Arbre;             use Cache_Arbre;
with SDA_Exceptions;          use SDA_Exceptions;
with Routeur_exceptions;      use Routeur_exceptions;

use Fonctions_globales.LCA_routeur_simple;

procedure routeur_la is
   
   Taille_Cache : Integer;
   Politique    : Tab_Politique;
   Statistique  : Boolean;
   Table        : Unbounded_String;
   Paquet       : Unbounded_String;
   Resultat     : Unbounded_String;
   
   Tab_Routage  : T_LCA;
   Cache        : T_Cache;
   
   Fichier_Entree  : File_Type;
   Fichier_Sortie  : File_Type;
   
   Ligne_Num : Integer := 0;
   
   Total_Hits : Integer := 0;
   Total_Misses : Integer := 0;
   
   function Politique_To_String(P : Tab_Politique) return String is
   begin
      case P is
         when FIFO => return "FIFO";
         when LRU => return "LRU";
         when LFU => return "LFU";
      end case;
   end Politique_To_String;
   
   function Traiter_Ligne(Ligne : Unbounded_String) return Boolean is
      Texte_Traite : Unbounded_String := Ligne;
      Adresse_IP   : T_Adresse_IP;
      Route_Trouvee : T_Case;
      Arret_Traitement : Boolean := False;
      Cache_Hit : Boolean := False;
   begin
      Trim(Texte_Traite, Ada.Strings.Both);
      
      if Length(Texte_Traite) = 0 then
         Arret_Traitement := False;
      else
         Ligne_Num := Ligne_Num + 1;
         
         declare
            Texte_Str : constant String := To_String(Texte_Traite);
         begin
            if Texte_Str'Length > 0 and then Texte_Str(1) in '0'..'9' then
               begin
                  Adresse_IP := Id_ad_IP(Texte_Str);
                  
                  begin
                     Route_Trouvee := Rechercher(Cache, Adresse_IP);
                     Cache_Hit := True;
                     Total_Hits := Total_Hits + 1;
                     
                     Ecrire(Fichier_Sortie, Adresse_IP, To_String(Route_Trouvee.Int));
                     
                  exception
                     when Cle_Absente_Error =>
                        Cache_Hit := False;
                        Total_Misses := Total_Misses + 1;
                        
                        Route_Trouvee := Trouver_Meilleure_Route(Tab_Routage, Adresse_IP);
                        
                        Enregistrer(Cache, Route_Trouvee.Destination, Route_Trouvee.Masque, Route_Trouvee.Int);
                        
                        Ecrire(Fichier_Sortie, Adresse_IP, To_String(Route_Trouvee.Int));
                  end;
                  
                  if Statistique then
                     Put_Line("Ligne " & Integer'Image(Ligne_Num) & ": " & 
                             (if Cache_Hit then "HIT" else "MISS") & 
                             " pour " & Texte_Str);
                  else
                     null;
                  end if;
                  
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
               if Texte_Str = "table" then
                  Afficher_table_routage(Tab_Routage);
                  Arret_Traitement := False;
                  
               elsif Texte_Str = "cache" then
                  Put_Line("=== Contenu du cache ===");
                  Afficher(Cache);
                  Put_Line("Taille: " & Integer'Image(Taille(Cache)) & 
                          "/" & Integer'Image(Taille_Cache));
                  Put_Line("Hits: " & Integer'Image(Total_Hits) & 
                          ", Misses: " & Integer'Image(Total_Misses));
                  Arret_Traitement := False;
                  
               elsif Texte_Str = "stat" then
                  if Statistique then
                     declare
                        Nb_Defauts, Nb_Demandes : Integer;
                        Taux_Defauts : Float;
                     begin
                        Obtenir_Statistiques(Cache, Nb_Defauts, Nb_Demandes, Taux_Defauts);
                        Put_Line("=== Statistiques du cache ===");
                        Put_Line("Politique: " & Politique_To_String(Politique));
                        Put("Demandes: "); Put(Nb_Demandes, 1); New_Line;
                        Put("Défauts:  "); Put(Nb_Defauts, 1); New_Line;
                        Put("Taux défauts: "); Put(Taux_Defauts, Fore => 1, Aft => 4, Exp => 0); New_Line;
                        Put_Line("Hits (manuels): " & Integer'Image(Total_Hits));
                        Put_Line("Misses (manuels): " & Integer'Image(Total_Misses));
                     end;
                  else
                     Put_Line("Statistiques non activées");
                  end if;
                  Arret_Traitement := False;
                  
               elsif Texte_Str = "fin" then
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
   
   Fin_Demandee : Boolean := False;
   
begin
   Gerer_commandes(Taille_Cache, Politique, Statistique, Table, Paquet, Resultat);
   
   Put_Line("=== Routeur avec Cache ===");
   Put_Line("Taille cache: " & Integer'Image(Taille_Cache));
   Put_Line("Politique: " & Politique_To_String(Politique));
   Put_Line("Table routage: " & To_String(Table));
   Put_Line("Fichier paquets: " & To_String(Paquet));
   Put_Line("Fichier résultats: " & To_String(Resultat));
   New_Line;
   
   Initialiser(Tab_Routage);
   Table_routage(To_String(Table), Tab_Routage);
   Put_Line("Table de routage chargée (" & Integer'Image(Taille(Tab_Routage)) & " routes)");
   
   Cache_Arbre.Initialiser(Cache, Taille_Cache, Politique);
   Put_Line("Cache initialisé (taille max: " & Integer'Image(Taille_Cache) & 
           ", politique: " & Politique_To_String(Politique) & ")");
   
   if Taille_Cache >= Taille(Tab_Routage) then
      Put_Line("ATTENTION: Cache plus grand ou égal à la table de routage (" & 
              Integer'Image(Taille(Tab_Routage)) & " routes).");
      Put_Line("Les politiques n'auront aucun effet car tout tiendra dans le cache.");
   else
      null;
   end if;
   New_Line;
   
   Ouvrir(To_String(Paquet), Fichier_Entree);
   Create(Fichier_Sortie, Out_File, To_String(Resultat));
   
   Put_Line("Traitement des paquets...");
   while not Fin_Demandee and not End_Of_File(Fichier_Entree) loop
      declare
         Ligne : constant Unbounded_String := 
            To_Unbounded_String(Get_Line(Fichier_Entree));
         Ligne_Fin : Boolean;
      begin
         Ligne_Fin := Traiter_Ligne(Ligne);
         if Ligne_Fin then
            Fin_Demandee := True;
         else
            null;
         end if;
      end;
   end loop;
   
   if Fin_Demandee then
      Put_Line("Arrêt du traitement suite à la commande 'fin'.");
   else
      Put_Line("Fin du fichier atteinte.");
   end if;
   
   Close(Fichier_Entree);
   Close(Fichier_Sortie);
   
   if Statistique then
      New_Line;
      Put_Line("=== Statistiques finales ===");
      Put_Line("Politique utilisée: " & Politique_To_String(Politique));
      Put_Line("Taille cache: " & Integer'Image(Taille_Cache));
      Put_Line("Routes dans table: " & Integer'Image(Taille(Tab_Routage)));
      
      declare
         Nb_Defauts, Nb_Demandes : Integer;
         Taux_Defauts : Float;
      begin
         Obtenir_Statistiques(Cache, Nb_Defauts, Nb_Demandes, Taux_Defauts);
         Put_Line("--- Statistiques internes du cache ---");
         Put_Line("Total demandes: " & Integer'Image(Nb_Demandes));
         Put_Line("Total défauts:  " & Integer'Image(Nb_Defauts));
         Put_Line("Taux défauts:   " & Float'Image(Taux_Defauts));
         
         Put_Line("--- Statistiques manuelles ---");
         Put_Line("Total Hits:     " & Integer'Image(Total_Hits));
         Put_Line("Total Misses:   " & Integer'Image(Total_Misses));
         if (Total_Hits + Total_Misses) > 0 then
            Put_Line("Taux hits:      " & 
                    Float'Image(Float(Total_Hits) / Float(Total_Hits + Total_Misses)));
         else
            null;
         end if;
      end;
   else
      null;
   end if;
   
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