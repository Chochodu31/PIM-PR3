with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Fonctions_globales;      use Fonctions_globales;

package Cache_Arbre is

   type T_Cache is private;  

   -- Initialiser un cache avec une taille maximale et une politique
   procedure Initialiser(Cache : out T_Cache; Taille_Max : Integer; Politique : Tab_Politique);

   -- Détruire le cache et libère la mémoire
   procedure Detruire(Cache : in out T_Cache);

   -- Retourner le nombre d'éléments dans le cache
   function Taille(Cache : T_Cache) return Integer;

   -- Rechercher une adresse IP dans le cache, lève Cle_Absente_Error si absente
   -- Retourne la route complète trouvée
   function Rechercher(Cache : in out T_Cache; Adresse_IP : T_Adresse_IP) return T_Case;

   -- Ajouter une route au cache
   procedure Enregistrer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP; Interf : Unbounded_String);

   -- Supprimer une route spécifique du cache
   procedure Supprimer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

   -- Mettre à jour une route selon la politique (LRU/LFU)
   procedure Mettre_A_Jour(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

   -- Afficher toutes les routes du cache
   procedure Afficher(Cache : in T_Cache);

   -- Retourner les statistiques d'utilisation du cache
   procedure Obtenir_Statistiques(Cache : T_Cache; Nb_Defauts, Nb_Demandes : out Integer; Taux : out Float);

private   

   type T_Noeud;
   type T_Trie is access T_Noeud;  

   type T_Tableau_Enfants is array (0 .. 1) of T_Trie;  -- Deux enfants (0/1 pour les bits)

   type T_Noeud is record
      Route      : T_Case;             -- Route stockée (Destination, Masque, Interface)
      Est_Route  : Boolean := False;   -- Vrai si ce nœud représente une route valide
      Enfants    : T_Tableau_Enfants := (others => null);  -- Enfants pour les bits 0 et 1
      Horloge    : Integer;            -- compteur pour FIFO/LRU
      Frequence  : Integer;            -- Compteur d'accès pour LFU
   end record;

   -- Structure du cache
   type T_Cache is record
      Racine        : T_Trie := null;   -- Racine de l'arbre
      Taille_Max    : Integer;          -- Taille maximale du cache
      Taille_Actuelle : Integer;        -- Nombre actuel de routes
      Horloge_Global : Integer;         -- Horloge globale 
      Nb_Defauts    : Integer;          -- Nombre de défauts de cache
      Nb_Demandes   : Integer;          -- Nombre total de demandes
      Politique     : Tab_Politique;    -- Politique de remplacement
   end record;

   -- Fonctions utilitaires internes
   function Trouver_Noeud(Racine : T_Trie; Destination : T_Adresse_IP; Longueur_Masque : Natural) return T_Trie;
   function Trouver_Plus_Ancien(Racine : T_Trie) return T_Trie;
   function Trouver_Moins_Frequent(Racine : T_Trie) return T_Trie;
   procedure Liberer_Sous_Arbre(Noeud : in out T_Trie);
   function Calculer_Longueur_Masque(Masque : T_Adresse_IP) return Natural;
   function Extraire_Bit(Adresse : T_Adresse_IP; Position : Natural) return Natural;

end Cache_Arbre;