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
end Fonctions_globales;