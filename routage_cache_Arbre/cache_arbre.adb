with Ada.Text_IO;             use Ada.Text_IO;
with SDA_Exceptions;          use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body Cache_Arbre is

   procedure Free is
      new Ada.Unchecked_Deallocation(Object => T_Noeud, Name => T_Trie);
      
   function Politique_To_String(P : Tab_Politique) return String is
   begin
      case P is
         when FIFO => return "FIFO";
         when LRU => return "LRU";
         when LFU => return "LFU";
      end case;
   end Politique_To_String;

   function Calculer_Longueur_Masque(Masque : T_Adresse_IP) return Natural is
      Longueur : Natural := 0;
      Temp : T_Adresse_IP := Masque;
   begin
      while Temp > 0 loop
         Longueur := Longueur + 1;
         Temp := Temp / 2;
      end loop;
      return Longueur;
   end Calculer_Longueur_Masque;

   function Extraire_Bit(Adresse : T_Adresse_IP; Position : Natural) return Natural is
   begin
      if Position > 31 then
         return 0;
      elsif (Adresse and T_Adresse_IP(2)**Position) /= 0 then
         return 1;
      else
         return 0;
      end if;
   end Extraire_Bit;

   procedure Initialiser(Cache : out T_Cache; Taille_Max : Integer; Politique : Tab_Politique) is
   begin
      Cache.Racine := null;
      Cache.Taille_Max := Taille_Max;
      Cache.Taille_Actuelle := 0;
      Cache.Horloge_Global := 0;
      Cache.Nb_Defauts := 0;
      Cache.Nb_Demandes := 0;
      Cache.Politique := Politique;
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
      Cache.Nb_Defauts := 0;
      Cache.Nb_Demandes := 0;
   end Detruire;

   function Taille(Cache : T_Cache) return Integer is
   begin
      return Cache.Taille_Actuelle;
   end Taille;

   function Trouver_Noeud(Racine : T_Trie; Destination : T_Adresse_IP; Longueur_Masque : Natural) return T_Trie is
      Noeud : T_Trie := Racine;
      Bit : Natural;
      Position : Integer := 31;
   begin
      while Position >= 0 and then Noeud /= null loop
         if Position < 32 - Longueur_Masque then
            return Noeud;
         end if;
         
         Bit := Extraire_Bit(Destination, Position);
         Noeud := Noeud.Enfants(Bit);
         Position := Position - 1;
      end loop;
      
      return Noeud;
   end Trouver_Noeud;

   function Rechercher(Cache : in out T_Cache; Adresse_IP : T_Adresse_IP) return T_Case is
      Noeud : T_Trie := Cache.Racine;
      Meilleure_Route : T_Case;
      Meilleure_Longueur : Natural := 0;
      Trouvee : Boolean := False;
      Bit : Natural;
      Position : Integer := 31;
   begin
      Cache.Nb_Demandes := Cache.Nb_Demandes + 1;

      while Position >= 0 and then Noeud /= null loop
         if Noeud.Est_Route then
            if (Adresse_IP and Noeud.Route.Masque) = Noeud.Route.Destination then
               declare
                  Longueur_Masque : Natural := Calculer_Longueur_Masque(Noeud.Route.Masque);
               begin
                  if Longueur_Masque > Meilleure_Longueur then
                     Meilleure_Longueur := Longueur_Masque;
                     Meilleure_Route := Noeud.Route;
                     Trouvee := True;
                  end if;
               end;
            end if;
         end if;
         
         Bit := Extraire_Bit(Adresse_IP, Position);
         Noeud := Noeud.Enfants(Bit);
         Position := Position - 1;
      end loop;
      
      if not Trouvee then
         Cache.Nb_Defauts := Cache.Nb_Defauts + 1;
         raise Cle_Absente_Error;
      end if;
      
      Mettre_A_Jour(Cache, Meilleure_Route.Destination, Meilleure_Route.Masque);
      
      return Meilleure_Route;
   end Rechercher;

   function Trouver_Plus_Ancien(Racine : T_Trie) return T_Trie is
      Resultat : T_Trie := null;
      Min_Horloge : Integer := Integer'Last;
      
      procedure Parcours(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route and then Noeud.Horloge < Min_Horloge then
               Min_Horloge := Noeud.Horloge;
               Resultat := Noeud;
            end if;
            Parcours(Noeud.Enfants(0));
            Parcours(Noeud.Enfants(1));
         else
            null;
         end if;
      end Parcours;
   begin
      Parcours(Racine);
      return Resultat;
   end Trouver_Plus_Ancien;

   function Trouver_Moins_Frequent(Racine : T_Trie) return T_Trie is
      Resultat : T_Trie := null;
      Min_Frequence : Integer := Integer'Last;
      Min_Horloge : Integer := Integer'Last;
      
      procedure Parcours(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route then
               if Noeud.Frequence < Min_Frequence or
                  (Noeud.Frequence = Min_Frequence and then Noeud.Horloge < Min_Horloge) then
                  Min_Frequence := Noeud.Frequence;
                  Min_Horloge := Noeud.Horloge;
                  Resultat := Noeud;
               end if;
            end if;
            Parcours(Noeud.Enfants(0));
            Parcours(Noeud.Enfants(1));
         else
            null;
         end if;
      end Parcours;
   begin
      Parcours(Racine);
      return Resultat;
   end Trouver_Moins_Frequent;

   procedure Supprimer_Route_Selon_Politique(Cache : in out T_Cache) is
      Noeud_A_Supprimer : T_Trie;
   begin
      if Cache.Racine = null or Cache.Taille_Actuelle = 0 then
         return;
      else
         case Cache.Politique is
            when FIFO | LRU =>
               Noeud_A_Supprimer := Trouver_Plus_Ancien(Cache.Racine);
            when LFU =>
               Noeud_A_Supprimer := Trouver_Moins_Frequent(Cache.Racine);
         end case;
         
         if Noeud_A_Supprimer /= null then
            Noeud_A_Supprimer.Est_Route := False;
            Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
         else
            null;
         end if;
      end if;
   end Supprimer_Route_Selon_Politique;

   procedure Enregistrer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP; Interf : Unbounded_String) is
      Longueur_Masque : Natural;
      Noeud : T_Trie;
      Bit : Natural;
      Position : Integer := 31;
   begin
      Longueur_Masque := Calculer_Longueur_Masque(Masque);
      
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Longueur_Masque);
      
      if Noeud /= null and then Noeud.Est_Route then
         Noeud.Route.Int := Interf;
         Noeud.Horloge := Cache.Horloge_Global;
         Cache.Horloge_Global := Cache.Horloge_Global + 1;
         return;
      end if;
      
      if Cache.Taille_Max > 0 and Cache.Taille_Actuelle >= Cache.Taille_Max then
         Supprimer_Route_Selon_Politique(Cache);
      end if;
      
      if Cache.Racine = null then
         Cache.Racine := new T_Noeud'(Route => (Destination => 0, Masque => 0, Int => Null_Unbounded_String),
                                     Est_Route => False,
                                     Enfants => (others => null),
                                     Horloge => 0,
                                     Frequence => 0);
      end if;
      
      Noeud := Cache.Racine;
      Position := 31;
      
      while Position >= 0 loop
         if Position < 32 - Longueur_Masque then
            exit;
         end if;
         
         Bit := Extraire_Bit(Destination, Position);
         
         if Noeud.Enfants(Bit) = null then
            Noeud.Enfants(Bit) := new T_Noeud'(Route => (Destination => 0, Masque => 0, Int => Null_Unbounded_String),
                                              Est_Route => False,
                                              Enfants => (others => null),
                                              Horloge => 0,
                                              Frequence => 0);
         end if;
         Noeud := Noeud.Enfants(Bit);
         Position := Position - 1;
      end loop;
      
      if not Noeud.Est_Route then
         Cache.Taille_Actuelle := Cache.Taille_Actuelle + 1;
      end if;
      
      Noeud.Est_Route := True;
      Noeud.Route := (Destination => Destination, Masque => Masque, Int => Interf);
      Noeud.Horloge := Cache.Horloge_Global;
      Noeud.Frequence := 0;
      Cache.Horloge_Global := Cache.Horloge_Global + 1;
   end Enregistrer;

   procedure Mettre_A_Jour(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP) is
      Longueur_Masque : Natural;
      Noeud : T_Trie;
   begin
      Longueur_Masque := Calculer_Longueur_Masque(Masque);
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Longueur_Masque);
      
      if Noeud /= null and then Noeud.Est_Route then
         case Cache.Politique is
            when LRU =>
               Noeud.Horloge := Cache.Horloge_Global;
               Cache.Horloge_Global := Cache.Horloge_Global + 1;
            when LFU =>
               Noeud.Frequence := Noeud.Frequence + 1;
            when FIFO =>
               null;
         end case;
      else
         null;
      end if;
   end Mettre_A_Jour;

   procedure Supprimer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP) is
      Longueur_Masque : Natural;
      Noeud : T_Trie;
   begin
      Longueur_Masque := Calculer_Longueur_Masque(Masque);
      Noeud := Trouver_Noeud(Cache.Racine, Destination, Longueur_Masque);
      
      if Noeud /= null and then Noeud.Est_Route then
         Noeud.Est_Route := False;
         Cache.Taille_Actuelle := Cache.Taille_Actuelle - 1;
      else
         null;
      end if;
   end Supprimer;

   procedure Afficher(Cache : T_Cache) is
      procedure Afficher_Noeud(Noeud : T_Trie) is
      begin
         if Noeud /= null then
            if Noeud.Est_Route then
               Afficher_Ad_IP(Noeud.Route.Destination);
               Put(" ");
               Afficher_Ad_IP(Noeud.Route.Masque);
               Put(" -> " & To_String(Noeud.Route.Int));
               Put(" (H:" & Integer'Image(Noeud.Horloge) & 
                   ", F:" & Integer'Image(Noeud.Frequence) & ")");
               New_Line;
            end if;
            Afficher_Noeud(Noeud.Enfants(0));
            Afficher_Noeud(Noeud.Enfants(1));
         else
            null;
         end if;
      end Afficher_Noeud;
   begin
      if Cache.Racine = null or Cache.Taille_Actuelle = 0 then
         Put_Line("Cache vide");
      else
         Put_Line("Contenu du cache (Politique: " & Politique_To_String(Cache.Politique) & "):");
         Afficher_Noeud(Cache.Racine);
      end if;
   end Afficher;

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