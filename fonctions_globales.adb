with Ada.Text_IO; use Ada.Text_IO;
with Routeur_exceptions; use Routeur_exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;

package body Fonctions_globales is
   
   procedure Afficher_Cle_Ad_IP(Cle: in Integer) is
   begin
      Put(Cle);
   end Afficher_Cle_Ad_IP;

   procedure Afficher_Donnee_Enregistrement(Val: in T_Case) is
   begin
      Put("Enregistrement");
   end Afficher_Donnee_Enregistrement;

   procedure initialiser_table (Table: in String; Tab_routage : in out T_LCA) is

      type T_Tab is array (1..3) of Unbounded_String;

      Entree : File_Type;
      Compteur_Espace : Boolean;
      Colonne : Integer;
      Texte: Unbounded_String;
      Enregistrement : T_Case;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Compteur : Integer;
      Tab : T_Tab;
      Int : Unbounded_String;
      Taille : Integer;      

   begin
      -- Initialiser la création de la table de routage
      begin
         Open (Entree, In_File, table);
      exception
         when Name_Error =>
            raise Fichier_Inconnu_Error;
      end;


      while not End_Of_File (Entree) loop
         -- Comprendre la ligne
         Texte := To_Unbounded_String(Get_Line(Entree));

         -- Décomposer en trois éléments         
         Colonne := 1;
         Compteur_Espace := False;

         for Compteur in 1..length(Texte) loop
            if Texte(Compteur) = ' ' and Compteur_Espace then
               null;
            elsif Texte(Compteur)= ' ' and not Compteur_Espace then
               compteur_espace := True;
               colonne := colonne +1;
            else 
               compteur_espace := False;
               Tab(colonne):= Tab(colonne) & Texte(compteur);                      
            end if;
         end loop;

         -- Ajouter à Tab_Routage
         Masque := id_ad_IP(Tab(2));
         Destination := id_ad_IP(Tab(1));
         Int := Unbounded_String(Tab(3));
         Enregistrement.Masque := Masque;
         Enregistrement.Destination:= Destination;
         Enregistrement.Int := Int;
         Taille := Taille(Sda);
         Ajouter(tab_routage, Taille + 1,Enregistrement);
      end loop;
      
      Close (Entree);
   end initialiser_table;

end Fonctions_globales;