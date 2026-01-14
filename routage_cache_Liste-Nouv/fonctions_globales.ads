with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with LISTES;
with Ada.Text_IO;		use Ada.Text_IO;

package Fonctions_globales is

   --  type Tab_Politique is (FIFO, LRU, LFU);
   type T_Adresse_IP is mod 2 ** 32;
   UN_OCTET: constant T_Adresse_IP := 2 ** 8;
   package Adresse_IP_IO is new Modular_IO (T_Adresse_IP);
   POIDS_FORT_ICI : constant T_Adresse_IP  := 2 ** 31;

   package Routeur_cache is new LISTES (
      T_interface => Unbounded_String,
      T_Adresse_IP => T_Adresse_IP,
      POIDS_FORT => POIDS_FORT_ICI
   );
   use Routeur_cache;

   procedure Afficher_int_ici(Int : in Unbounded_String);

   procedure Afficher_Ad_IP (M1 : in T_Adresse_IP);
   
   procedure Afficher_R_C is new Afficher_Liste(
      Afficher_Adresse_IP => Afficher_Ad_IP,
      Afficher_Int => Afficher_int_ici
   );

   function inf_ici(Masque_nouv : in T_Adresse_IP; Masque : in T_Adresse_IP) return Boolean;
   function Et_ici (Adresse_IP : in T_Adresse_IP; Masque : in T_Adresse_IP) return T_Adresse_IP;
   procedure Rien_ici(Masque : in out T_Adresse_IP);
   procedure Rien_Interface_Ici(Inter : in out Unbounded_String);
   procedure Asso is new association_liste(
      Et => Et_ici,
      inf => inf_ici,
      Rien => Rien_ici,
      Rien_Interface => Rien_Interface_Ici
   );

   function Produit_ici(Destination : in T_Adresse_IP; bit : in Integer) return T_Adresse_IP;
   procedure Asso_ici(Masque : in out T_Adresse_IP; Int : in Integer);
   
   procedure Dest is new Dest_Masq_Max(
      Et => Et_ici,
      Produit => produit_ici,
      Rien => Rien_ici,
      Association_int => Asso_ici
   );


   procedure Ajout_cache(Routage: in T_Liste; Cellule : in T_Liste; Adresse_IP : in T_Adresse_IP; Cache : in out T_Liste; politique : in Tab_politique; Cache_Taille : in Integer; Masque : in out T_Adresse_IP);


   -- Créer la table de routage à partir d'un fichier
   -- Exception : Fichier_Inconnu_Error si Table n'est pas un fichier ouvrable
   procedure Table_routage (Table : in String; Tab_routage : in out T_Liste);


   -- Analyser les arguments de la ligne de commande
   -- Exception : Commande_Inconnu_Error si la ligne de commande ne respecte pas les critères demandés
   procedure Gerer_commandes (Cache : out Integer; 
                              Politique : out Tab_Politique; 
                              Statistique : out Boolean; 
                              Table : out Unbounded_String;
                              Paquet : out Unbounded_String;
                              Resultat : out Unbounded_String
                              );


   -- Ouvrir fichier
   -- Exception : Fichier_Inconnu_Error si le fichier qu'on essaye d'ouvrir n'existe pas.
   procedure Ouvrir (Paquet : in String; Entree : in out File_Type);


   -- Traiter les paquets à router
   procedure Traiter_les_paquets (Entree : in File_Type; Sortie : in out File_Type; Tab_routage : in T_Liste; Cache : in out T_Liste; Politique : in Tab_Politique ; Cache_Taille : in integer; Nb_demande : out Integer; Nb_defaut : out Integer);
   
   function Id_ad_IP(Texte : in String) return T_Adresse_IP;
   
   function association_ad_des (Cache : in out T_Liste; Tab_Routage : in T_Liste; Adresse_IP : in T_Adresse_IP; Politique : in Tab_Politique; Cache_Taille : in integer; Nb_defaut : in out Integer) return Unbounded_String;
   
   procedure Identifier_commande (Texte : in String; Ligne : in Integer; Tab_routage : in T_Liste; Cache : in T_Liste);
   
end Fonctions_globales;