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
   procedure Ecrire_Ad_IP(Sortie: in out File_Type; M1 : in T_Adresse_IP);
   procedure Afficher_Ad_IP(M1 : T_Adresse_IP);
   procedure Afficher_Cle_Ad_IP(Cle: in Integer);
   procedure Afficher_Donnee_Enregistrement(Val: in T_Case);

   procedure Afficher_Debug_routeur_simpe is new Afficher_Debug(
      Afficher_Cle => Afficher_Cle_Ad_IP,
      Afficher_Donnee => Afficher_Donnee_Enregistrement
   );

   procedure table_routage(Table: in String; Tab_routage : in out T_LCA);

   function id_ad_IP(Texte : in String) return T_Adresse_IP;

   procedure Gerer_commandes (Cache: out Integer; 
                              Politique : out Tab_Politique; 
                              Statistique : out Boolean; 
                              Table : out Unbounded_String;
                              Paquet : out Unbounded_String;
                              Resultat : out Unbounded_String );
   
   procedure association_ad_des(Tab_Routage : in T_LCA; Sortie : in out File_Type; Adresse_IP : in T_Adresse_IP);

   procedure identifier_commande (Texte : in String; Ligne : in Integer; Tab_routage : in T_LCA);

end Fonctions_globales;