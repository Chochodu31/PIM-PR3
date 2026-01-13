with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Fonctions_globales;      use Fonctions_globales;

package Cache_Arbre is

   type T_Cache is private;  

   -- Initialiser un cache avec une taille maximale
   procedure Initialiser(Cache : out T_Cache; Taille_Max : Integer);

   -- Détruire le cache et libère la mémoire
   procedure Detruire(Cache : in out T_Cache);

   -- Retourner le nombre d'éléments dans le cache
   function Taille(Cache : T_Cache) return Integer;

   -- Rechercher une adresse IP dans le cache, lève Cle_Absente_Error si absente
   function Rechercher(Cache : in out T_Cache; Adresse_IP : T_Adresse_IP) return Unbounded_String;

   -- Ajouter une route au cache
   procedure Enregistrer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP; Interf : Unbounded_String);

   -- Supprimer une route spécifique du cache
   procedure Supprimer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

   -- Supprimer la route la plus ancienne (politique FIFO)
   procedure Supprimer_FIFO(Cache : in out T_Cache);

   -- Supprimer la route la moins récemment utilisée (politique LRU)
   procedure Supprimer_LRU(Cache : in out T_Cache);

   -- Supprimer la route la moins fréquemment utilisée (politique LFU)
   procedure Supprimer_LFU(Cache : in out T_Cache);

   -- Mettre à jour clk d'une route (pour LRU)
   procedure Mettre_A_Jour_LRU(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

   -- Incrémenter le compteur d'accès d'une route (pour LFU)
   procedure Mettre_A_Jour_LFU(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

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
      Clk        : Integer;            -- compteur pour FIFO/LRU
      Frequence  : Integer;            -- Compteur d'accès pour LFU
   end record;

   -- Structure du cache
   type T_Cache is record
      Racine        : T_Trie := null;   -- Racine de l'arbre
      Taille_Max    : Integer;          -- Taille maximale du cache
      Taille_Actuelle : Integer;        -- Nombre actuel de routes
      Horloge       : Integer;          -- Horloge globale 
      Nb_Defauts    : Integer;          -- Nombre de défauts de cache
      Nb_Demandes   : Integer;          -- Nombre total de demandes
   end record;

   -- Fonctions utilitaires internes
   function Trouver_Noeud(Racine : T_Trie; Destination, Masque : T_Adresse_IP) return T_Trie;
   function Trouver_Plus_Ancien(Racine : T_Trie) return T_Trie;
   procedure Liberer_Sous_Arbre(Noeud : in out T_Trie);

end Cache_Arbre;