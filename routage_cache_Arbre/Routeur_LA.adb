with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Fonctions_globales;        use Fonctions_globales;
use Fonctions_globales.LCA_routeur_simple;
with Cache_Arbre;               use Cache_Arbre;

procedure Routeur_LA is

   Cache_Taille   : Integer;           
   Politique      : Tab_Politique;     
   Statistique    : Boolean;        
   Table          : Unbounded_String;  
   Paquet         : Unbounded_String;  
   Resultat       : Unbounded_String;  
   Tab_routage    : T_LCA;           
   Cache          : T_Cache;          
   Entree         : File_Type;        
   Sortie         : File_Type;         
   
   Ligne_Compteur : Integer := 0;     
   Fin_Traitement : Boolean := False;  

   function Calculer_Masque_Adapte (Destination : T_Adresse_IP; Masque_Route : T_Adresse_IP) return T_Adresse_IP is
   begin
      return Masque_Route;
   end Calculer_Masque_Adapte;

   function Trouver_Route (Adresse_IP : T_Adresse_IP) return T_Case is
      Masque_Max : T_Adresse_IP := 0;
      Meilleure_Route : T_Case;
      Route_Temp : T_Case;
      Trouvee : Boolean := False;
   begin
      for I in 1 .. Taille(Tab_routage) loop
         begin
            Route_Temp := La_Valeur(Tab_routage, I);
            if (Adresse_IP and Route_Temp.Masque) = Route_Temp.Destination then
               if Route_Temp.Masque >= Masque_Max then
                  Masque_Max := Route_Temp.Masque;
                  Meilleure_Route := Route_Temp;
                  Trouvee := True;
               end if;
            end if;
         exception
            when Cle_Absente_Error => null;
         end;
      end loop;
      
      if not Trouvee then
         raise Adresse_IP_Introuvable_Error;
      end if;
      
      return Meilleure_Route;
   end Trouver_Route;

   -- Traiter une ligne de commande
   procedure Traiter_Commande (Texte : in String; Ligne : in Integer) is
   begin
      if Texte = "table" then
         Put_Line("table (ligne" & Integer'Image(Ligne) & ")");
         Afficher_table_routage(Tab_routage);
         
      elsif Texte = "cache" then
         Put_Line("cache (ligne" & Integer'Image(Ligne) & ")");
         Afficher(Cache);
         
      elsif Texte = "stat" then
         Put_Line("stat (ligne" & Integer'Image(Ligne) & ")");
         declare
            Defauts, Demandes : Integer;
            Taux : Float;
         begin
            Obtenir_Statistiques(Cache, Defauts, Demandes, Taux);
            Put("Défauts de cache : ");
            Put(Defauts, 1);
            New_Line;
            Put("Demandes de route : ");
            Put(Demandes, 1);
            New_Line;
            Put("Taux de défauts : ");
            Put(Float'Image(Taux));
            New_Line;
         end;
         
      elsif Texte = "fin" then
         Put_Line("fin (ligne" & Integer'Image(Ligne) & ")");
         Fin_Traitement := True;
         
      else
         Put_Line("Erreur : commande inconnue à la ligne" & Integer'Image(Ligne));
         raise Commande_Inconnu_Error;
      end if;
   end Traiter_Commande;

   -- Traiter une adresse IP avec cache
   procedure Traiter_Adresse_IP (Adresse_IP_Str : in String) is
      Adresse_IP : T_Adresse_IP;
      Interface_Trouvee : Unbounded_String;
      Route : T_Case;
      Masque_Adapte : T_Adresse_IP;
   begin
      -- Convertir l'adresse IP
      Adresse_IP := Id_ad_IP(Adresse_IP_Str);
      
      -- Incrémenter le compteur de demandes
      Cache.Nb_Demandes := Cache.Nb_Demandes + 1;
      
      -- Chercher dans le cache d'abord
      begin
         Interface_Trouvee := Rechercher(Cache, Adresse_IP);
         -- Cache hit : mettre à jour selon politique
         if Politique = LRU then
            -- Pour LRU, on devrait mettre à jour le timestamp
            -- Mais notre Rechercher ne retourne pas la route exacte
            null; -- À améliorer si nécessaire
         elsif Politique = LFU then
            null; -- À améliorer si nécessaire
         end if;
         
      exception
         when Cle_Absente_Error =>
            Cache.Nb_Defauts := Cache.Nb_Defauts + 1;
            
            -- Chercher dans la table de routage
            Interface_Trouvee := Association_ad_des(Tab_routage, Adresse_IP);
            
            -- Trouver la route complète pour obtenir destination et masque
            Route := Trouver_Route(Adresse_IP);
            
            -- Calculer le masque adapté pour le cache
            declare
               Masque_Adapte := Calculer_Masque_Adapte(Adresse_IP, Route.Masque);
            begin
               -- Vérifier si le cache est plein
               if Taille(Cache) >= Cache.Taille_Max then
                  -- Supprimer une entrée selon la politique
                  case Politique is
                     when FIFO => Supprimer_FIFO(Cache);
                     when LRU => Supprimer_LRU(Cache);
                     when LFU => Supprimer_LFU(Cache);
                  end case;
               end if;
               
               -- Enregistrer la route dans le cache
               Enregistrer(Cache, Route.Destination, Masque_Adapte, Interface_Trouvee);
            end;
      end;
      Ecrire(Sortie, Adresse_IP, To_String(Interface_Trouvee));
   end Traiter_Adresse_IP;

   -- Traiter toutes les lignes du fichier d'entrée
   procedure Traiter_Fichier is
      Texte : Unbounded_String;
   begin
      while not End_Of_File(Entree) and then not Fin_Traitement loop
         Texte := To_Unbounded_String(Get_Line(Entree));
         Ligne_Compteur := Ligne_Compteur + 1;
         Trim(Texte, Both);
         
         if Length(Texte) > 0 then
            if Element(Texte, 1) in '0' .. '9' then
               Traiter_Adresse_IP(To_String(Texte));
            else
               Traiter_Commande(To_String(Texte), Ligne_Compteur);
            end if;
         end if;
      end loop;
   end Traiter_Fichier;

begin
   -- Analyser la ligne de commande
   Gerer_commandes(Cache_Taille, Politique, Statistique, Table, Paquet, Resultat);

   -- Initialiser la table de routage
   Initialiser(Tab_routage);
   Table_routage(To_String(Table), Tab_routage);

   -- Initialiser le cache
   Initialiser(Cache, Cache_Taille);

   -- Ouvrir les fichiers
   Create(Sortie, Out_File, To_String(Resultat));
   Ouvrir(To_String(Paquet), Entree);

   -- Traiter le fichier
   Traiter_Fichier;

   -- Fermer les fichiers
   Close(Entree);
   Close(Sortie);

   -- Afficher les statistiques 
   if Statistique then
      New_Line;
      Put_Line("=== Statistiques ===");
      Put("Taille du cache : ");
      Put(Cache_Taille, 1);
      New_Line;
      Put("Politique utilisée : ");
      case Politique is
         when FIFO => Put("FIFO");
         when LRU => Put("LRU");
         when LFU => Put("LFU");
      end case;
      New_Line;
      
      declare
         Defauts, Demandes : Integer;
         Taux : Float;
      begin
         Obtenir_Statistiques(Cache, Defauts, Demandes, Taux);
         Put("Nombre de demandes : ");
         Put(Demandes, 1);
         New_Line;
         Put("Nombre de défauts : ");
         Put(Defauts, 1);
         New_Line;
         Put("Taux de défauts : ");
         if Demandes > 0 then
            Put(Float'Image(Taux * 100.0) & "%");
         else
            Put("0.0%");
         end if;
         New_Line;
      end;
   end if;
   Detruire(Tab_routage);
   Detruire(Cache);

exception
   when Fichier_Inconnu_Error =>
      Put_Line("Erreur : fichier introuvable");
   when Commande_Inconnu_Error =>
      Put_Line("Erreur : commande inconnue");
   when others =>
      Put_Line("Erreur inattendue");
      if Is_Open(Entree) then
         Close(Entree);
      end if;
      if Is_Open(Sortie) then
         Close(Sortie);
      end if;
      raise;
end Routeur_LA;