with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with SDA_Exceptions;          use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body Cache_Arbre is

   procedure Free is
      new Ada.Unchecked_Deallocation(Object => T_Noeud, Name => T_Trie);

   -- Initialiser un cache vide avec la taille maximale spécifiée
   procedure Initialiser(Cache : out T_Cache; Taille_Max : Integer) is
   begin
      Cache.Racine := null;
      Cache.Taille_Max := Taille_Max;
      Cache.Taille_Actuelle := 0;
      Cache.Horloge := 0;
      Cache.Nb_Defauts := 0;
      Cache.Nb_Demandes := 0;
   end Initialiser;

   -- Libèrer récursivement un sous-arbre
   procedure Liberer_Sous_Arbre(Noeud : in out T_Trie) is
   begin
      if Noeud /= null then
         Liberer_Sous_Arbre(Noeud.Enfants(0));
         Liberer_Sous_Arbre(Noeud.Enfants(1));
         Free(Noeud);
      end if;
   end Liberer_Sous_Arbre;

   -- Détruire le cache en libérant toute la mémoire
   procedure Detruire(Cache : in out T_Cache) is
   begin
      Liberer_Sous_Arbre(Cache.Racine);
      Cache.Racine := null;
      Cache.Taille_Actuelle := 0;
   end Detruire;

   -- Retourner le nombre de routes actuellement dans le cache
   function Taille(Cache : T_Cache) return Integer is
   begin
      return Cache.Taille_Actuelle;
   end Taille;

   -- Rechercher une adresse IP dans le cache en suivant les bits du masque
   function Rechercher(Cache : in out T_Cache; Adresse_IP : T_Adresse_IP) return Unbounded_String is
      Noeud : T_Trie := Cache.Racine;
      Resultat : Unbounded_String := Null_Unbounded_String;
      Bit : Integer;
      I : Integer := 31;
      Fin_Parcours : Boolean := False;
   begin
      -- Incrémenter le compteur de demandes
      Cache.Nb_Demandes := Cache.Nb_Demandes + 1;
      
      -- Parcourir les 32 bits de l'adresse IP
      while I >= 0 and then not Fin_Parcours loop
         -- Déterminer le bit courant (0 ou 1)
         if (Adresse_IP and (2 ** I)) /= 0 then
            Bit := 1;
         else
            Bit := 0;
         end if;
         
         if Noeud = null then
            Fin_Parcours := True;
         else
            -- Si ce noeud contient une route, on la mémorise (masque le plus long)
            if Noeud.Est_Route then
               Resultat := Noeud.Route.Int;
            end if;
            Noeud := Noeud.Enfants(Bit);
            I := I - 1;
         end if;
      end loop;
      
      -- Si aucune route trouvée, c'est un défaut de cache
      if Resultat = Null_Unbounded_String then
         Cache.Nb_Defauts := Cache.Nb_Defauts + 1;
         raise Cle_Absente_Error;
      else
         return Resultat;
      end if;
   end Rechercher;

   -- Ajouter une route au cache en suivant les bits de destination
   procedure Enregistrer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP; Interf : Unbounded_String) is
      Noeud : T_Trie := Cache.Racine;
      Bit : Integer;
      Longueur_Masque : Integer := 0;
      Courant : T_Adresse_IP := Masque;
      I : Integer;
   begin
      -- Calculer la longueur du masque (nombre de bits à 1)
      while Courant /= 0 loop
         Longueur_Masque := Longueur_Masque + 1;
         Courant := Courant / 2;
      end loop;
      
      -- Créer la racine si nécessaire
      if Noeud = null then
         Cache.Racine := new T_Noeud'(Route => (Destination => Destination, Masque => Masque, Int => Interf), 
                                     Est_Route => False, Enfants => (others => null), Clk => 0, Frequence => 0);
         Noeud := Cache.Racine;
      end if;
      
      -- Parcourir les bits significatifs (selon la longueur du masque)
      I := 31;
      while I >= 32 - Longueur_Masque loop
         if (Destination and (2 ** I)) /= 0 then
            Bit := 1;
         else
            Bit := 0;
         end if;
         
         -- Créer un nœud enfant si nécessaire
         if Noeud.Enfants(Bit) = null then
            Noeud.Enfants(Bit) := new T_Noeud'(Route => (Destination => Destination, Masque => Masque, Int => Interf), 
                                               Est_Route => False, Enfants => (others => null), Clk => 0, Frequence => 0);
         end if;
         Noeud := Noeud.Enfants(Bit);
         I := I - 1;
      end loop;
      
      -- Marquer le dernier noeud comme contenant une route valide
      Noeud.Est_Route := True;
      Noeud.Route := (Destination => Destination, Masque => Masque, Int => Interf);
      Noeud.Clk := Cache.Horloge;
      Noeud.Frequence := 0;
      Cache.Horloge := Cache.Horloge + 1;
      Cache.Taille_Actuelle := Cache.Taille_Actuelle + 1;
   end Enregistrer;

   -- Supprimer la route la plus ancienne (plus petit Clk)
   procedure Supprimer_FIFO(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie;
   begin
      if Cache.Racine /= null then
         Noeud_A_Supprimer := Trouver_Plus_Ancien(Cache.Racine);
         if Noeud_A_Supprimer /= null then
            Noeud_A_Supprimer.Est_Route := False;
            Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
         end if;
      end if;
   end Supprimer_FIFO;

   -- Parcourt l'arbre pour trouver le noeud avec le plus petit Clk
   function Trouver_Plus_Ancien(Racine : T_Trie) return T_Trie is
      Resultat : T_Trie := null;
      Min_Clk : Integer := Integer'Last;
      
      procedure Parcours(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route and then Noeud.Clk < Min_Clk then
               Min_Clk := Noeud.Clk;
               Resultat := Noeud;
            end if;
            Parcours(Noeud.Enfants(0));
            Parcours(Noeud.Enfants(1));
         end if;
      end Parcours;
      
   begin
      Parcours(Racine);
      return Resultat;
   end Trouver_Plus_Ancien;

   -- Mettre à jour clk d'une route (pour LRU)
   procedure Mettre_A_Jour_LRU(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null then
         Noeud.Clk := Cache.Horloge;
         Cache.Horloge := Cache.Horloge + 1;
      end if;
   end Mettre_A_Jour_LRU;

   -- Trouver un nœud spécifique (Destination + Masque)
   function Trouver_Noeud(Racine : T_Trie; Destination, Masque : T_Adresse_IP) return T_Trie is
      Resultat : T_Trie := null;
      
      procedure Parcours(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route and then Noeud.Route.Destination = Destination 
               and then Noeud.Route.Masque = Masque then
               Resultat := Noeud;
            else
               Parcours(Noeud.Enfants(0));
               Parcours(Noeud.Enfants(1));
            end if;
         end if;
      end Parcours;
      
   begin
      Parcours(Racine);
      return Resultat;
   end Trouver_Noeud;

   -- Pour LRU, même méthode de suppression que FIFO (mais Clk est mis à jour à chaque accès)
   procedure Supprimer_LRU(Cache : in out T_Cache) is
   begin
      Supprimer_FIFO(Cache);
   end Supprimer_LRU;

   -- Supprimer la route la moins fréquemment utilisée
   procedure Supprimer_LFU(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie;
      Min_Frequence : Integer := Integer'Last;
      
      procedure Trouver_Moins_Frequent_Rec(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route then
               if Noeud.Frequence < Min_Frequence then
                  Min_Frequence := Noeud.Frequence;
                  Noeud_A_Supprimer := Noeud;
               elsif Noeud.Frequence = Min_Frequence and then 
                     (Noeud_A_Supprimer = null or else Noeud.Clk < Noeud_A_Supprimer.Clk) then
                  Noeud_A_Supprimer := Noeud;  -- En cas d'égalité, le plus ancien
               end if;
            end if;
            Trouver_Moins_Frequent_Rec(Noeud.Enfants(0));
            Trouver_Moins_Frequent_Rec(Noeud.Enfants(1));
         end if;
      end Trouver_Moins_Frequent_Rec;
      
   begin
      if Cache.Racine /= null then
         Trouver_Moins_Frequent_Rec(Cache.Racine);
         if Noeud_A_Supprimer /= null then
            Noeud_A_Supprimer.Est_Route := False;
            Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
         end if;
      end if;
   end Supprimer_LFU;

   -- Incrémenter le compteur d'accès d'une route (pour LFU)
   procedure Mettre_A_Jour_LFU(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null then
         Noeud.Frequence := Noeud.Frequence + 1;
      end if;
   end Mettre_A_Jour_LFU;

   -- Afficher toutes les routes du cache
   procedure Afficher(Cache : T_Cache) is
      procedure Afficher_Noeud(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route then
               Afficher_Ad_IP(Noeud.Route.Destination);
               Put(" ");
               Afficher_Ad_IP(Noeud.Route.Masque);
               Put(" ");
               Put(To_String(Noeud.Route.Int));
               New_Line;
            end if;
            Afficher_Noeud(Noeud.Enfants(0));
            Afficher_Noeud(Noeud.Enfants(1));
         end if;
      end Afficher_Noeud;
   begin
      Afficher_Noeud(Cache.Racine);
   end Afficher;

   -- Supprimer une route spécifique
   procedure Supprimer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null then
         Noeud.Est_Route := False;
         Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
      end if;
   end Supprimer;

   -- Retourner les statistiques d'utilisation du cache
   procedure Obtenir_Statistiques(Cache : T_Cache; Nb_Defauts, Nb_Demandes : out Integer; Taux : out Float) is
   begin
      Nb_Defauts := Cache.Nb_Defauts;
      Nb_Demandes := Cache.Nb_Demandes;
      if Nb_Demandes > 0 then
         Taux := Float(Nb_Defauts) / Float(Nb_Demandes);
      else
         Taux := 0.0;
      end if;
   end Obtenir_Statistiques;

end Cache_Arbre;