with Ada.Strings;               use Ada.Strings;	-- pour Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;

function initialiser_table(table: in string; Tab_routage : in out T_LCA) return t_sda is
   Entree : file_type;
   compteur_espace : Boolean;
   colonne : integer;
   texte: Unbounded_String;
   Enregistrement : T_Case;
   destination : t_adress_ip;
   masque : t_adress_ip;
   type t_table is array(1..3) of Unbounded_String;
   tab : T_table;
     
begin
   begin
      Open (Entree, In_File, table);
   exception
      when Name_Error =>
         raise Fichier_Inconnu_Error;
   end;
   while not End_Of_File (Entree) loop
      Texte := To_Unbounded_String(Get_Line (Entree));
      for I in 1..length(tab) loop
         Tab(I) := To_Unbounded_String("");
      end loop;
      colonne := 1;
      compteur_Espace := False;

      for compteur in 1..length(Texte) loop


         if Texte(Compteur)= ' ' and compteur_espace then
            null;
         elsif Texte(Compteur)= ' ' and not compteur_espace then
            null;
            compteur_espace := True;
            colonne:=colonne +1;
         else 
            compteur_espace :=False;
            tab(colonne):= Tab(colonne) & texte(compteur);                      
         end if;
      end loop;

      masque := id_ad_IP(tab(2));
      Destination := id_ad_IP(Tab(1));
      Interface_r := Unbounded_string(Tab(3));
      Enregistrement.masque := masque;
      Enregistrement.Destination:=Destination;
      taille := Taille(Sda);
      Ajouter(tab_routage,Taille+1,Enregistrement);
   end loop;
   Close (Entree);
   return tab_routage;

end initialiser_table;
