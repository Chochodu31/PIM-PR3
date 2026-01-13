with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Fonctions_globales;      use Fonctions_globales;

package Cache_Arbre is

   procedure Initialiser(Cache : out T_Cache; Taille_Max : Integer);

   procedure Detruire(Cache : in out T_Cache);

   function Taille(Cache : T_Cache) return Integer;

   -- Recherche d'une adresse IP dans le cache
   -- Retourne l'interface si trouvée, sinon lève Cle_Absente_Error
   function Rechercher(Cache : T_Cache; Adresse_IP : T_Adresse_IP) return Unbounded_String;

   -- Ajout d'une route dans le cache
   procedure Enregistrer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP; Interface : Unbounded_String);

   -- Suppression d'une route spécifique
   procedure Supprimer(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

   -- Suppression selon politique FIFO (plus ancien)
   procedure Supprimer_FIFO(Cache : in out T_Cache);

   -- Suppression selon politique LRU 
   procedure Supprimer_LRU(Cache : in out T_Cache);

   -- Suppression selon politique LFU
   procedure Supprimer_LFU(Cache : in out T_Cache);

   -- Mise à jour des données pour LRU
   procedure Mettre_A_Jour_LRU(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

   -- Mise à jour métadonnées pour LFU (incrémente la fréquence)
   procedure Mettre_A_Jour_LFU(Cache : in out T_Cache; Destination, Masque : T_Adresse_IP);

   -- Affichage du cache (pour commande "cache")
   procedure Afficher(Cache : in T_Cache);

   -- Statistiques
   procedure Obtenir_Statistiques(Cache : T_Cache; Nb_Defauts, Nb_Demandes : out Integer; Taux : out Float);

   -- Fonction utilitaire : parcours l'arbre pour trouver un nœud spécifique
   function Trouver_Noeud(Racine : T_Trie; 
                          Destination, Masque : T_Adresse_IP) 
      return T_Trie;
   
   -- Fonction pour trouver le nœud avec le plus petit Clk (pour FIFO/LRU)
   function Trouver_Plus_Ancien(Racine : T_Trie) return T_Trie;
   
   -- Fonction pour trouver le nœud avec la plus petite fréquence (pour LFU)
   function Trouver_Moins_Frequent(Racine : T_Trie) return T_Trie;
   
   -- Libération récursive d'un sous-arbre
   procedure Liberer_Sous_Arbre(Noeud : in out T_Trie);

      type T_Noeud;
   type T_Trie is access T_Noeud;

private   

   -- Un nœud de l'arbre
   type T_Noeud is record
      Route      : T_Case;             -- Destination, Masque, Interface
      Est_Route  : Boolean := False;   -- Vrai si ce nœud est une route valide dans le cache
      Enfants    : array (0 .. 1) of T_Trie := (others => null);
      Clk        : Integer ;            -- date de modification pour LRU
      Frequence  : Integer ;            -- Compteur d'accès pour LFU
   end record;

   -- Le cache (arbre)
   type T_Cache is record
      Racine        : T_Trie := null;
      Taille_Max    : Integer;
      Taille_Actuelle : Integer ;
      Horloge       : Integer ;  -- Horloge globale incrémentée à chaque événement
      Nb_Defauts    : Integer ;
      Nb_Demandes   : Integer ;
   end record;

end Cache_Arbre;