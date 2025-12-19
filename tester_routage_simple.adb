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

   generic
      K1, K2, K3, K4, K5 : Integer; -- 5 entrée dans dans la table de routage
      V1, V2, V3, V4, V5 : T_Case;

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


   procedure Tester_Gerer_Commande is 
   begin
      Put("Tester_Gerer_Commande : ");
      Put("Ok");
   end Tester_Gerer_Commande;

   procedure Tester_Ouvrir is 
   begin
      Put("Tester_Ouvrir : ");
      Put("Ok");
   end Tester_Ouvrir;

   procedure Tester_Traiter_les_Paquets is
   begin
      Put("Tester_Traiter_les_Paquets : ");
      Put("Ok");
   end Tester_Traiter_les_Paquets;

   procedure Tester_Id_ad_IP is 
   begin
      Put("Tester_ID_ad_IP : ");
      Put("Ok");
   end Tester_Id_ad_IP;

   procedure Tester_Association_ad_des is
   begin
      Put("Tester_Association_ad_des : ");
      Put("Ok");
   end Tester_Association_ad_des;

   procedure Tester_Identifier_commande is
   begin
      Put("Tester_Identifier_commande : ");
      Put("Ok");
   end Tester_Identifier_commande;

   package Testeur_Integer_Case is new Testeur (
      1, 2, 3, 4, 5,
      Case1, Case2, Case3, Case4, Case5
   );

   procedure Tester_Tout_Integer_Case is new Testeur_Integer_Case.Tester_Tout;
begin
   Tester_Tout_Integer_Case;
   New_Line;
   Put_Line ("Tout les tests ont réussi.");
end Testeur;