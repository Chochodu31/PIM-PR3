with Ada.Text_IO;             use Ada.Text_IO;
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
   begin
      -- Incrémenter le compteur de demandes
      Cache.Nb_Demandes := Cache.Nb_Demandes + 1;

      -- Parcourir l'arbre pour trouver le prefixe le plus long (LPM)
      while I >= 0 and then Noeud /= null loop
         -- Si ce noeud contient une route, on la mémorise
         if Noeud.Est_Route then
            Resultat := Noeud.Route.Int;
         end if;

         -- Déterminer le bit courant
         if (Adresse_IP and T_Adresse_IP(2)**I) /= 0 then
            Bit := 1;
         else
            Bit := 0;
         end if;
         
         Noeud := Noeud.Enfants(Bit);
         I := I - 1;
      end loop;

      -- Vérification finale pour le dernier noeud atteint
      if Noeud /= null and then Noeud.Est_Route then
         Resultat := Noeud.Route.Int;
      end if;

      -- Gestion du résultat et des statistiques
      if Resultat = Null_Unbounded_String then
         Cache.Nb_Defauts := Cache.Nb_Defauts + 1;
         raise Cle_Absente_Error;
      else
         return Resultat;
      end if;
   end Rechercher;

   -- Ajouter une route au cache en suivant les bits de destination
   procedure Enregistrer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP; Interf : Unbounded_String) is
      Noeud : T_Trie;
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
      
      -- Vérifier si la route existe déjà
      declare
         Noeud_Existant : constant T_Trie := Trouver_Noeud(Cache.Racine, Destination, Masque);
      begin
         if Noeud_Existant /= null and then Noeud_Existant.Est_Route then
            -- Route existante : mise à jour simple de l'interface
            Noeud_Existant.Route.Int := Interf;
            return;
         end if;
      end;

      -- Si le cache est plein, supprimer une route (FIFO par défaut pour les tests)
      if Cache.Taille_Actuelle >= Cache.Taille_Max and Cache.Taille_Max > 0 then
         Supprimer_FIFO(Cache);
      end if;
      
      -- Créer la racine si nécessaire
      if Cache.Racine = null then
         Cache.Racine := new T_Noeud'(Route => (others => <>), Est_Route => False, 
                                     Enfants => (others => null), Clk => 0, Frequence => 0);
      end if;
      
      Noeud := Cache.Racine;
      I := 31;
      -- Parcourir/Créer les bits significatifs selon le masque
      while I >= 32 - Longueur_Masque loop
         if (Destination and T_Adresse_IP(2)**I) /= 0 then
            Bit := 1;
         else
            Bit := 0;
         end if;

         if Noeud.Enfants(Bit) = null then
            Noeud.Enfants(Bit) := new T_Noeud'(Route => (others => <>), Est_Route => False, 
                                               Enfants => (others => null), Clk => 0, Frequence => 0);
         end if;
         Noeud := Noeud.Enfants(Bit);
         I := I - 1;
      end loop;
      
      -- Marquer le noeud final comme une route
      if not Noeud.Est_Route then
         Cache.Taille_Actuelle := Cache.Taille_Actuelle + 1;
      end if;
      
      Noeud.Est_Route := True;
      Noeud.Route := (Destination => Destination, Masque => Masque, Int => Interf);
      Noeud.Clk := Cache.Horloge;
      Noeud.Frequence := 0;
      Cache.Horloge := Cache.Horloge + 1;
   end Enregistrer;

   -- Supprimer la route la plus ancienne (plus petit Clk)
   procedure Supprimer_FIFO(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie;
   begin
      if Cache.Racine /= null and Cache.Taille_Actuelle > 0 then
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
      if Noeud /= null and then Noeud.Est_Route then
         Noeud.Clk := Cache.Horloge;
         Cache.Horloge := Cache.Horloge + 1;
      end if;
   end Mettre_A_Jour_LRU;

   -- Trouver un nœud spécifique (Destination + Masque) en suivant le chemin binaire
   function Trouver_Noeud(Racine : T_Trie; Destination, Masque : T_Adresse_IP) return T_Trie is
      Noeud : T_Trie := Racine;
      L : Integer := 0;
      Temp_Masque : T_Adresse_IP := Masque;
      Bit : Integer;
   begin
      while Temp_Masque /= 0 loop
         L := L + 1;
         Temp_Masque := Temp_Masque / 2;
      end loop;

      for I in reverse 32-L .. 31 loop
         if Noeud = null then return null; end if;
         if (Destination and T_Adresse_IP(2)**I) /= 0 then
            Bit := 1;
         else
            Bit := 0;
         end if;
         Noeud := Noeud.Enfants(Bit);
      end loop;
      return Noeud;
   end Trouver_Noeud;

   -- Supprimer la route la moins récemment utilisée (politique LRU)
   procedure Supprimer_LRU(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie;
   begin
      if Cache.Racine /= null and Cache.Taille_Actuelle > 0 then
         Noeud_A_Supprimer := Trouver_Plus_Ancien(Cache.Racine);
         if Noeud_A_Supprimer /= null then
            Noeud_A_Supprimer.Est_Route := False;
            Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
         end if;
      end if;
   end Supprimer_LRU;

   -- Supprimer la route la moins fréquemment utilisée
   procedure Supprimer_LFU(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie := null;
      Min_Frequence : Integer := Integer'Last;
      Min_Clk : Integer := Integer'Last;

      procedure Trouver_Moins_Frequent_Rec(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route then
               if Noeud.Frequence < Min_Frequence or
                  (Noeud.Frequence = Min_Frequence and then Noeud.Clk < Min_Clk) then
                  Min_Frequence := Noeud.Frequence;
                  Min_Clk := Noeud.Clk;
                  Noeud_A_Supprimer := Noeud;
               end if;
            end if;
            Trouver_Moins_Frequent_Rec(Noeud.Enfants(0));
            Trouver_Moins_Frequent_Rec(Noeud.Enfants(1));
         end if;
      end Trouver_Moins_Frequent_Rec;
   begin
      if Cache.Racine /= null and Cache.Taille_Actuelle > 0 then
         Trouver_Moins_Frequent_Rec(Cache.Racine);
         if Noeud_A_Supprimer /= null then
            Noeud_A_Supprimer.Est_Route := False;
            Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
         end if;
      end if;
   end Supprimer_LFU;

   procedure Mettre_A_Jour_LFU(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null and then Noeud.Est_Route then
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
      if Cache.Racine = null or Cache.Taille_Actuelle = 0 then
         Put_Line("Cache vide");
      else
         Afficher_Noeud(Cache.Racine);
      end if;
   end Afficher;

   -- Supprimer une route spécifique
   procedure Supprimer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP) is
      Noeud : T_Trie;
   begin
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Masque);
      if Noeud /= null and then Noeud.Est_Route then
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