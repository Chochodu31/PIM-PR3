with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with LCA;
with Ada.Text_IO;		use Ada.Text_IO;

package Fonctions_globales is

   type Tab_Politique is (FIFO, LRU, LFU);

   type T_Adresse_IP is mod 2 ** 32;
   
   type T_Case is record
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Int : Unbounded_String;
   end record;


   package Adresse_IP_IO is new Modular_IO (T_Adresse_IP);
   

   package LCA_routeur_simple is new LCA (
      T_Cle => Integer,
      T_Valeur => T_Case
   );
   use LCA_routeur_simple;


   -- Afficher une clé (Integer).
   procedure Afficher_Cle_Integer (Cle : in Integer);


   -- Afficher une donnée de la table de routage
   procedure Afficher_Donnee_Case (Val : in T_Case);


   -- Afficher la table de routage
   -- Exemple d'affichage : 
   -- -->[1 : (32.248.90.0, 255.255.255.0, eth1)]-->[2 : (32.248.0.0, 255.255.0.0, eth2)]-->[3 : (0.0.0.0, 0.0.0.0, eth0)]--E
   procedure Afficher_table_routage is new Afficher_Debug (
      Afficher_Cle => Afficher_Cle_Integer,
      Afficher_Donnee => Afficher_Donnee_Case
   );


   -- Convertir une chaine en T_Adresse_IP
   -- Exception : Adresse_IP_Introuvable_Error si échec de transformation
   function Id_ad_IP (Texte : in String) return T_Adresse_IP;


   -- Créer la table de routage à partir d'un fichier
   -- Exception : Fichier_Inconnu_Error si Table n'est pas un fichier ouvrable
   procedure Table_routage (Table : in String; Tab_routage : in out T_LCA);


   -- Analyser les arguments de la ligne de commande
   -- Exception : Commande_Inconnu_Error si la ligne de commande ne respecte pas les critères demandés
   procedure Gerer_commandes (Cache : out Integer; 
                              Politique : out Tab_Politique; 
                              Statistique : out Boolean; 
                              Table : out Unbounded_String;
                              Paquet : out Unbounded_String;
                              Resultat : out Unbounded_String
                              );


   procedure Ouvrir(Paquet : in String; Entree : in out File_Type);

   -- Traiter les paquets à router
   procedure Traiter_les_paquets (Entree : in File_Type; Sortie : in out File_Type; Tab_routage : in T_LCA);


   -- Association de l'adresse IP et de Destination dans la table de routage.
   -- Exception : Adresse_IP_Introuvable_Error si il n'y à pas de Destination et de Masque qui correspondent à l'adresse IP
   function Association_ad_des (Tab_Routage : in T_LCA; Adresse_IP : in T_Adresse_IP) return Unbounded_String;
   

   -- Ecrire dans le fichier de Sortie l'adressse IP et l'interface associé
   procedure Ecrire (Sortie : in out File_Type; Adresse_IP : in T_Adresse_IP; Int : in String);


   -- Identifier la de commande écrite
   -- Exception : Commande_Inconnu_Error si la ligne de commande ne respecte pas les critères demandés
   procedure Identifier_commande (Texte : in String; Ligne : in Integer; Tab_routage : in T_LCA);

end Fonctions_globales;