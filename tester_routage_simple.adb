with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Fonctions_globales;   use Fonctions_globales;
use Fonctions_globales.LCA_routeur_simple;
with ada.Integer_Text_IO;  use ada.Integer_Text_IO;
with Routeur_exceptions; use Routeur_exceptions;
with Ada.Command_Line;		use Ada.Command_Line;
with Sda_Exceptions;		use Sda_Exceptions;
with Ada.Strings; 		use Ada.Strings;   
with LCA;

procedure Tester_routage_simple is

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

   package Testeur is

      generic
         with function "+" (Gauche, Droite : in V) return V;
      procedure Tester_Tout;

      procedure Tester_Table_Routage;

      procedure Tester_Gerer_Commande;

      procedure Tester_Ouvrir;

      procedure Tester_Traiter_les_Paquets;

      procedure Tester_Id_ad_IP;

      procedure Tester_Association_ad_des;

      procedure Tester_Identifier_commande;

   end Testeur;

   package body Testeur is
      use LCA_routeur_simple;

      procedure Tester_Tout is
      begin
         Tester_Table_Routage;
         Tester_Gerer_Commande;
         Tester_Ouvrir;
         Tester_Traiter_les_Paquets;
         Tester_Id_ad_IP;
         Tester_Association_ad_des;
         Tester_Identifier_commande;

         Put_Line ("Tests : OK.");
      end Tester_Tout;
   
   end Testeur;

   procedure Creer_Table_routage is
   begin
   end Creer_Table_routage;

   procedure Creer_Paquet is 
   begin
   end Creer_Paquet;

   procedure Tester_Table_Routage is
      Table : String;
      Tab_routage : T_LCA;
   begin
      Put("Tester_Table_Routage : ");
      Table_routage (Table, Tab_routage);
      Put ("OK");
   end Tester_Table_Routage;

begin

end Testeur;