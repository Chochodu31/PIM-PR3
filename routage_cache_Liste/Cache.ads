with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

package Cache is

   type T_Adresse_IP is mod 2 ** 32;
   UN_OCTET : constant T_Adresse_IP := 2 ** 8;
   type T_Politique_Cache is (FIFO, LRU, LFU);

   Cache_Vide_Error : exception;
   Taille_Cache_Error : exception;

   -- Type du cash liste chaînée
   type T_Cache is limited private;   
   
   -- Affiche le contenu du cache
   --  procedure Afficher_Cache(Cache : T_Cache);

   -- Initialiser un cache vide
   --  procedure Initialiser(Cache : out T_Cache);
   
   -- Détruire un cache et libérer la mémoire
   --  procedure Detruire(Cache : in out T_Cache);
   
   -- Retourne la taille actuelle du cache
   --  function Taille(Cache : T_Cache) return Integer;
   
   -- Vérifie si le cache est vide
   --  function Est_Vide(Cache : T_Cache) return Boolean;   

   -- Recherche une adresse IP dans le cache ,retourne l'interface si trouvée, sinon chaine vide
   function Rechercher_Dans_Cache(
      Cache : in out T_Cache;
      Adresse_IP : T_Adresse_IP;
      Politique : T_Politique_Cache;
      Compteur_Global : in out Integer
   ) return Unbounded_String;
   
   -- Vérifie si une route (Destination + Masque) est dans le cache
   function Route_Dans_Cache(
      Cache : T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP
   ) return Boolean;

      -- Ajoute une entrée au cache selon la politique et la taille maximale
   procedure Ajouter_Au_Cache(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Interface : Unbounded_String;
      Politique : T_Politique_Cache;
      Taille_Max : Integer;
      Compteur_Global : in out Integer
   );

   -- Supprime une entrée du cache selon la politique
   procedure Supprimer_Du_Cache(
      Cache : in out T_Cache;
      Politique : T_Politique_Cache;
      Compteur_Global : Integer
   );

   -- Met à jour les donnees du cache
   procedure Mettre_A_Jour_Cache(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Politique : T_Politique_Cache;
      Compteur_Global : Integer
   );

   -- Incremente la fréquence d'une entrée par 1 (cas LFU)
   procedure Incrementer_Frequence(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP
   );

   -- Déplace une entrée en tête du cache (cas LRU)
   procedure Deplacer_En_Tete(
      Cache : in out T_Cache;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP
   );

private

   type T_Cellule;
   type T_Cache is access T_Cellule;
   
   type T_Cellule is record
      Destination : T_Adresse_IP;    
      Masque      : T_Adresse_IP;    
      Interface   : Unbounded_String; 
      Frequence   : Integer;          
      Suivant     : T_Cache;          
   end record;


end Cache; 

