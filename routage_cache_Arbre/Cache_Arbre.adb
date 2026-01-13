with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with SDA_Exceptions;          use SDA_Exceptions;
with fonctions_globales;      use fonctions_globales;
with Ada.Unchecked_Deallocation;

package body Cache_Arbre is

   procedure Free is
      new Ada.Unchecked_Deallocation(Object => T_Noeud, Name => T_Trie);

   procedure Initialiser(Cache : out T_Cache; Taille_Max : Integer) is
   begin
      Cache.Racine := null;
      Cache.Taille_Max := Taille_Max;
      Cache.Taille_Actuelle := 0;
      Cache.Horloge := 0;
      Cache.Nb_Defauts := 0;
      Cache.Nb_Demandes := 0;
   end Initialiser;

   procedure Liberer_Sous_Arbre(Noeud : in out T_Trie) is
   begin
      if Noeud /= null then
         Liberer_Sous_Arbre(Noeud.Enfants(0));
         Liberer_Sous_Arbre(Noeud.Enfants(1));
         Free(Noeud);
      else
         null;
      end if;
   end Liberer_Sous_Arbre;

   procedure Detruire(Cache : in out T_Cache) is
   begin
      Liberer_Sous_Arbre(Cache.Racine);
      Cache.Racine := null;
      Cache.Taille_Actuelle := 0;
   end Detruire;

   function Taille(Cache : T_Cache) return Integer is
   begin
      return Cache.Taille_Actuelle;
   end Taille;

   -- Recherche dans l'arbre (parcours bit par bit)
   function Rechercher(Cache : T_Cache; Adresse_IP : T_Adresse_IP) return Unbounded_String is
      Noeud : T_Trie := Cache.Racine;
      Resultat : Unbounded_String := Null_Unbounded_String;
      Bit : Integer;
      I : Integer := 31;
      Fin_Parcours : Boolean := False;
   begin
      -- Parcourir les 32 bits
      while I >= 0 and then not Fin_Parcours loop
         if (Adresse_IP and (2 ** I)) /= 0 then
            Bit := 1;
         else
            Bit := 0;
         end if;
         
         if Noeud = null then
            Fin_Parcours := True;
         else
            -- Si ce noued contient une route, la memoriser (masque plus long)
            if Noeud.Est_Route then
               Resultat := Noeud.Route.Int;
            else
               null;
            end if;
            Noeud := Noeud.Enfants(Bit);
            I := I - 1;
         end if;
      end loop;
      
      if Resultat = Null_Unbounded_String then
         raise Cle_Absente_Error;
      else
         return Resultat;
      end if;
   end Rechercher;

   -- Insertion d'une route
   procedure Enregistrer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP; Interface : Unbounded_String) is
      Noeud : T_Trie := Cache.Racine;
      Bit : Integer;
      Longueur_Masque : Integer := 0;
      Courant : T_Adresse_IP := Masque;
      I : Integer;
   begin
      -- Calculer la longueur du masque
      while Courant /= 0 loop
         Longueur_Masque := Longueur_Masque + 1;
         Courant := Courant / 2;
      end loop;
      
      if Noeud = null then
         Cache.Racine := new T_Noeud'(Route => (Destination, Masque, Interface), Est_Route => False, Enfants => (null, null), Clk => 0, Frequence => 0);
         Noeud := Cache.Racine;
      else
         null;
      end if;
      
      I := 31;
      while I >= 32 - Longueur_Masque loop
         if (Destination and (2 ** I)) /= 0 then
            Bit := 1;
         else
            Bit := 0;
         end if;
         
         if Noeud.Enfants(Bit) = null then
            Noeud.Enfants(Bit) := new T_Noeud'(Route => (Destination, Masque, Interface), Est_Route => False, Enfants => (null, null), Clk => 0, Frequence => 0);
            Noeud := Noeud.Enfants(Bit);
         else
            Noeud := Noeud.Enfants(Bit);
         end if;
         I := I - 1;
      end loop;
      -- Marquer le noeud comme contenant une route
      Noeud.Est_Route := True;
      Noeud.Route := (Destination, Masque, Interface);
      Noeud.Clk := Cache.Horloge;
      Noeud.Frequence := 0;
      Cache.Horloge := Cache.Horloge + 1;
      Cache.Taille_Actuelle := Cache.Taille_Actuelle + 1;
   end Enregistrer;

   -- Suppression selon politique FIFO
   procedure Supprimer_FIFO(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie;
      Min_Clk : Integer := Cache.Horloge;
   begin
      if Cache.Racine = null then
         null;
      else
         -- Trouver le noeud avec le plus petit Clk (le plus ancien)
         Noeud_A_Supprimer := Trouver_Plus_Ancien(Cache.Racine);
         
         if Noeud_A_Supprimer /= null then
            Noeud_A_Supprimer.Est_Route := False;
            Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
         else
            null;
         end if;
      end if;
   end Supprimer_FIFO;

   -- Trouver le noeud avec le plus petit Clk (pour FIFO/LRU)
   function Trouver_Plus_Ancien(Racine : T_Trie) return T_Trie is
      Resultat : T_Trie := null;
      Min_Clk : Integer := Integer'Last;
      procedure Parcours(Noeud : T_Trie) is
      begin
         if Noeud = null then
            null;
         else
            if Noeud.Est_Route then
               if Noeud.Clk < Min_Clk then
                  Min_Clk := Noeud.Clk;
                  Resultat := Noeud;
               else
                  null;
               end if;
            else
               null;
            end if; 
            Parcours(Noeud.Enfants(0));
            Parcours(Noeud.Enfants(1));
         end if;
      end Parcours;
      
   begin
      Parcours(Racine);
      return Resultat;
   end Trouver_Plus_Ancien;

   -- Mise à jour pour LRU
   procedure Mettre_A_Jour_LRU(Cache : in out T_Cache; 
                               Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null then
         Noeud.Clk := Cache.Horloge;
         Cache.Horloge := Cache.Horloge + 1;
      else
         null;
      end if;
   end Mettre_A_Jour_LRU;

   -- Trouver un noeud spécifique (Destination + Masque)
   function Trouver_Noeud(Racine : T_Trie; 
                          Destination, Masque : T_Adresse_IP) 
      return T_Trie is
      Resultat : T_Trie := null;
      
      procedure Parcours(Noeud : T_Trie) is
      begin
         if Noeud = null then
            null;
         else
            if Noeud.Est_Route then
               if Noeud.Route.Destination = Destination and then
                  Noeud.Route.Masque = Masque then
                  Resultat := Noeud;
               else
                  null;
               end if;
            else
               null;
            end if;
            
            Parcours(Noeud.Enfants(0));
            Parcours(Noeud.Enfants(1));
         end if;
      end Parcours;
      
   begin
      Parcours(Racine);
      return Resultat;
   end Trouver_Noeud;

   -- Suppression selon politique LRU
   procedure Supprimer_LRU(Cache : in out T_Cache) is
   begin
      -- Pour LRU, on supprime aussi le nœud avec le plus petit Clk
      -- La seule différence est que pour LRU, le Clk est mis à jour à chaque accès
      -- donc le plus ancien = celui avec le plus petit Clk
      Supprimer_FIFO(Cache);
   end Supprimer_LRU;

   -- Suppression selon politique LFU
   procedure Supprimer_LFU(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie;
      Min_Frequence : Integer := Integer'Last;
      
      procedure Trouver_Moins_Frequent_Rec(Noeud : T_Trie) is
      begin
         if Noeud = null then
            null;
         else
            if Noeud.Est_Route then
               if Noeud.Frequence < Min_Frequence then
                  Min_Frequence := Noeud.Frequence;
                  Noeud_A_Supprimer := Noeud;
               elsif Noeud.Frequence = Min_Frequence then
                  -- En cas d'égalité, on prend le plus ancien (plus petit Clk)
                  if Noeud_A_Supprimer = null or else Noeud.Clk < Noeud_A_Supprimer.Clk then
                     Noeud_A_Supprimer := Noeud;
                  else
                     null;
                  end if;
               else
                  null;
               end if;
            else
               null;
            end if;
            
            Trouver_Moins_Frequent_Rec(Noeud.Enfants(0));
            Trouver_Moins_Frequent_Rec(Noeud.Enfants(1));
         end if;
      end Trouver_Moins_Frequent_Rec;
      
   begin
      if Cache.Racine = null then
         null;
      else
         Trouver_Moins_Frequent_Rec(Cache.Racine);
         
         if Noeud_A_Supprimer /= null then
            Noeud_A_Supprimer.Est_Route := False;
            Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
         else
            null;
         end if;
      end if;
   end Supprimer_LFU;

   -- Mise à jour pour LFU
   procedure Mettre_A_Jour_LFU(Cache : in out T_Cache; 
                               Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null then
         Noeud.Frequence := Noeud.Frequence + 1;
      else
         null;
      end if;
   end Mettre_A_Jour_LFU;

   -- Affichage du cache
   procedure Afficher(Cache : T_Cache) is
      procedure Afficher_Noeud(Noeud : T_Trie) is
      begin
         if Noeud = null then
            null;
         else
            if Noeud.Est_Route then
               Afficher_Ad_IP(Noeud.Route.Destination);
               Put(" ");
               Afficher_Ad_IP(Noeud.Route.Masque);
               Put(" ");
               Put(To_String(Noeud.Route.Int));
               New_Line;
            else
               null;
            end if;
            
            Afficher_Noeud(Noeud.Enfants(0));
            Afficher_Noeud(Noeud.Enfants(1));
         end if;
      end Afficher_Noeud;
   begin
      Afficher_Noeud(Cache.Racine);
   end Afficher;

   -- Suppression d'une route spécifique
   procedure Supprimer(Cache : in out T_Cache; 
                       Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null then
         Noeud.Est_Route := False;
         Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
      else
         null;
      end if;
   end Supprimer;

   -- Statistiques
   procedure Obtenir_Statistiques(Cache : T_Cache; 
                                  Defauts, Demandes : out Integer; 
                                  Taux : out Float) is
   begin
      Defauts := Cache.Nb_Defauts;
      Demandes := Cache.Nb_Demandes;
      if Demandes > 0 then
         Taux := Float(Defauts) / Float(Demandes);
      else
         Taux := 0.0;
      end if;
   end Obtenir_Statistiques;

end Cache_Arbre;