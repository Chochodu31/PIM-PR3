with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Cache is

   procedure Free is new Ada.Unchecked_Deallocation(T_Cellule, T_Cache);

   procedure Initialiser(Cache : out T_Cache) is
   begin
      Cache := null;
   end Initialiser;

   procedure Detruire(Cache : in out T_Cache) is
      Cache_Temp : T_Cache;
   begin
      while Cache /= null loop
         Cache_Temp := Cache;
         Cache := Cache.Suivant;
         Free(Cache_Temp);
      end loop;
      Cache := null;
   end Detruire;   

   function Taille(Cache : T_Cache) return Integer is
      Compteur : Integer := 0;
      Cache_Temp : T_Cache := Cache;
   begin
      while Cache_Temp /= null loop
         Compteur := Compteur + 1;
         Cache_Temp := Cache_Temp.Suivant;
      end loop;
      return Compteur;
   end Taille;

   function Est_Vide(Cache : T_Cache) return Boolean is
   begin
      return Cache = null;
   end Est_Vide;   

   function Route_Dans_Cache(
      Cache : T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP
   ) return Boolean is
      Courant : T_Cache := Cache;
   begin
      while Courant /= null loop
         if Courant.Destination = Destination and Courant.Masque = Masque then
            return True;
         else
            Null;   
         end if;
         Courant := Courant.Suivant;
      end loop;
      return False;
   end Route_Dans_Cache;

   procedure Incrementer_Frequence(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP
   ) is
      Courant : T_Cache := Cache;
   begin
      while Courant /= null loop
         if Courant.Destination = Destination and Courant.Masque = Masque then
            Courant.Frequence := Courant.Frequence + 1;
         else
            Null;
         end if;
         Courant := Courant.Suivant;
      end loop;
   end Incrementer_Frequence;

   procedure Deplacer_En_Tete(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP
   ) is
      Courant : T_Cache := Cache;
      Trouver : T_Cache := null;
      Prev_Trouver : T_Cache := null;
   begin
      while Courant /= null loop
         if Courant.Destination = Destination and Courant.Masque = Masque then
            Trouver := Courant;
            Prev_Trouver := Trouver;
         else
            Null;
         end if;
         Prev_Trouver := Courant;
         Courant := Courant.Suivant;
      end loop;
      -- Si l'élément existe mais pas en tête
      if Trouver /= null and then Prev_Trouver /= null then
         -- Retirer l'élément de sa position
         Prev_Trouver.Suivant := Trouver.Suivant;
         -- Placer l'élément en tête
         Trouver.Suivant := Cache;
         Cache := Trouver;
      else
         null;
      end if;
   end Deplacer_En_Tete;               

   procedure Mettre_A_Jour_Cache(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Politique : T_Politique_Cache;
      Compteur_Global : Integer
   ) is
         Courant : T_Cache := Cache;
   begin
      case Politique is
         when FIFO =>
            -- FIFO ne met a jour rien
            null;
         when LRU =>
            begin
               while Courant /= null loop
                  if Courant.Destination = Destination and Courant.Masque = Masque then
                     Courant.Frequence := Compteur_Global;
                  else
                     null;
                  end if;
                  Courant := Courant.Suivant;
               end loop;
            end;
         when LFU =>
            -- incrémenter la fréquence pour LFU
            Incrementer_Frequence(Cache, Destination, Masque);
      end case;
   end Mettre_A_Jour_Cache;

   procedure Supprimer_Du_Cache(
      Cache : in out T_Cache;
      Politique : T_Politique_Cache;
      Compteur_Global : Integer
   ) is
      Courant : T_Cache := Cache;
      Prev : T_Cache := null;
      A_Retirer : T_Cache := null;
      Prev_A_Retirer : T_Cache := null;
      Min_Freq : Integer;
   begin
      if Cache = null then
         raise Cache_Vide_Error;
      end if;
      
      case Politique is
         when FIFO =>
            -- Supprimer le premier élément (le plus ancien)
            A_Retirer := Cache;
            Cache := Cache.Suivant;
            Free(A_Retirer);
         when LRU =>
            Min_Freq := Integer'Last;
            Courant := Cache;
            Prev := null;    
            while Courant /= null loop
               if Courant.Frequence < Min_Freq then
                  Min_Freq := Courant.Frequence;
                  A_Retirer := Courant;
                  Prev_A_Retirer := Prev;
               else
                  null;
               end if;
               Prev := Courant;
               Courant := Courant.Suivant;
            end loop;
            if A_Retirer /= null then
               if Prev_A_Retirer = null then
                  -- C'est la tête de liste
                  Cache := A_Retirer.Suivant;
               else
                  Prev_A_Retirer.Suivant := A_Retirer.Suivant;
               end if;
               Free(A_Retirer);
            end if;
         when LFU =>
            -- Trouver l'élément avec la plus petite fréquence
            Min_Freq := Integer'Last;
            Courant := Cache;
            Prev := null;
            
            while Courant /= null loop
               if Courant.Frequence < Min_Freq then
                  Min_Freq := Courant.Frequence;
                  A_Retirer := Courant;
                  Prev_A_Retirer := Prev;
               end if;
               Prev := Courant;
               Courant := Courant.Suivant;
            end loop;
            -- Supprimer l'élément trouvé
            if A_Retirer /= null then
               if Prev_A_Retirer = null then
                  -- C'est la tête de liste
                  Cache := A_Retirer.Suivant;
               else
                  Prev_A_Retirer.Suivant := A_Retirer.Suivant;
               end if;
               Free(A_Retirer);
            end if;
      end case;
   end Supprimer_Du_Cache;

   function Rechercher_Dans_Cache(
      Cache : in out T_Cache;
      Adresse_IP : T_Adresse_IP;
      Politique : T_Politique_Cache;
      Compteur_Global : in out Integer
   ) return Unbounded_String is
      Courant : T_Cache := Cache;
   begin
      while Courant /= null loop
         -- Vérifier si l'adresse IP correspond à la route
         if (Adresse_IP and Courant.Masque) = Courant.Destination then
            -- Mettre à jour selon la politique
            Mettre_A_Jour_Cache(Cache, Courant.Destination, Courant.Masque, Politique, Compteur_Global);
            Compteur_Global := Compteur_Global + 1;
            return Courant.Interface;
         end if;
         Courant := Courant.Suivant;
      end loop;
      
      -- Aucune correspondance trouvée
      return To_Unbounded_String("");
   end Rechercher_Dans_Cache;

   procedure Ajouter_Au_Cache(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Interface : Unbounded_String;
      Politique : T_Politique_Cache;
      Taille_Max : Integer;
      Compteur_Global : in out Integer
   ) is
      New_Cell : T_Cache;
   begin
      -- Vérifier si la route existe déjà
      if Route_Dans_Cache(Cache, Destination, Masque) then
         -- Mettre à jour le cache
         Mettre_A_Jour_Cache(Cache, Destination, Masque, Politique, Compteur_Global);
         return;
      end if;
      -- Vérifier si le cache est plein
      if Taille(Cache) >= Taille_Max then
         -- Supprimer une entrée selon la politique
         Supprimer_Du_Cache(Cache, Politique, Compteur_Global);
      end if;
      -- Créer une nouvelle cellule
      New_Cell := new T_Cellule'(
         Destination => Destination,
         Masque => Masque,
         Interface => Interface,
         Frequence => 1,
         Suivant => Cache  -- Ajout en tête
      );
      -- Initialiser la fréquence selon la politique
      case Politique is
         when FIFO =>
            New_Cell.Frequence := 0;
         when LRU =>
            New_Cell.Frequence := Compteur_Global;
         when LFU =>
            New_Cell.Frequence := 1;
      end case;
     -- Ajouter en tête de liste
      Cache := New_Cell;
      
      -- Incrémenter le compteur global (pour LRU)
      Compteur_Global := Compteur_Global + 1;
   end Ajouter_Au_Cache;                  

   procedure Afficher_Cache(Cache : T_Cache) is
      temp : T_Cache := Cache;
      Compteur : Integer := 1;
   begin
      Put_Line("-> Contenu du Cache ");
      while temp /= null loop
         Put("Element " & Integer'Image(Compteur) & ": ");
         Put("Destination=");
         Put(Natural'Image(Natural((temp.Destination / UN_OCTET ** 3) mod UN_OCTET)) & "." &
             Natural'Image(Natural((temp.Destination / UN_OCTET ** 2) mod UN_OCTET)) & "." &
             Natural'Image(Natural((temp.Destination / UN_OCTET ** 1) mod UN_OCTET)) & "." &
             Natural'Image(Natural(temp.Destination mod UN_OCTET)));
         Put(", Masque=");
         Put(Natural'Image(Natural((temp.Masque / UN_OCTET ** 3) mod UN_OCTET)) & "." &
             Natural'Image(Natural((temp.Masque / UN_OCTET ** 2) mod UN_OCTET)) & "." &
             Natural'Image(Natural((temp.Masque / UN_OCTET ** 1) mod UN_OCTET)) & "." &
             Natural'Image(Natural(temp.Masque mod UN_OCTET)));
         Put(", Interface=" & To_String(temp.Interface));
         Put_Line(", Frequence=" & Integer'Image(temp.Frequence));
         
         temp := temp.Suivant;
         Compteur := Compteur + 1;
      end loop;                            
      if Est_Vide(Cache) then
         Put_Line("Cache vide");
      end if;
   end Afficher_Cache;


end Cache;



