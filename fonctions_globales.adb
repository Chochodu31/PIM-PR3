with Routeur_exceptions; use Routeur_exceptions;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;

package body Fonctions_globales is
   type T_Octet is mod 2 ** 8;
   UN_OCTET: constant T_Adresse_IP := 2 ** 8;
   

   procedure Afficher_Cle_Ad_IP(Cle: in Integer) is
   begin
      Put(Cle);
   end Afficher_Cle_Ad_IP;


   procedure Afficher_Donnee_Enregistrement(Val: in T_Case) is
   begin
      Put("(");
      --  Put(Val.Destination);
      --  Put(", ");
      --  Put(Val.Masque);
      --  Put(", ");
      Put(To_String(Val.Int));
   end Afficher_Donnee_Enregistrement;


   procedure table_routage (Table: in String; Tab_routage : in out T_LCA) is
      type T_Tab is array (1..3) of Unbounded_String;
      Entree : File_Type;
      Compteur_Espace : Boolean;
      Colonne : Integer;
      Texte: Unbounded_String;
      Enregistrement : T_Case;
      Destination : T_Adresse_IP;
      Masque : T_Adresse_IP;
      --  Compteur : Integer;
      Tab : T_Tab;
      Int : Unbounded_String;
      Taille_Var : Integer;      

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
            if To_String(Texte)(Compteur) = ' ' and Compteur_Espace then
               null;
            elsif To_String(Texte)(Compteur)= ' ' and not Compteur_Espace then
               compteur_espace := True;
               colonne := colonne +1;
            else 
               compteur_espace := False;
               Tab(colonne):= Tab(colonne) & To_String(Texte)(compteur);                      
            end if;
         end loop;

         -- Ajouter à Tab_Routage
         Masque := id_ad_IP(To_String(Tab(2)));
         Destination := id_ad_IP(To_String(Tab(1)));
         Int := Tab(3);
         Enregistrement.Masque := Masque;
         Enregistrement.Destination:= Destination;
         Enregistrement.Int := Int;
         Taille_Var := Taille(Tab_routage);
         Enregistrer(tab_routage, Taille_Var + 1, Enregistrement);
      end loop;
      
      Close (Entree);
   end table_routage;



   function id_ad_IP(Texte : in String) return T_Adresse_IP is
      indice_octet : Integer := 1;
      valeur_courante : Integer := 0;
      c : Character;
      type T_Tableau_Octets is array (1..4) of T_Octet;
      octets : T_Tableau_Octets;
      adresse_IP : T_Adresse_IP := 0;
   begin
      for i in 1..Length(To_Unbounded_String(Texte)) loop
         c := Texte(i);
         if c = '.' then
            if indice_octet > 3 then
               raise Adresse_IP_Introuvable_Error;
            end if;   
            if valeur_courante > 255 then
               raise Adresse_IP_Introuvable_Error;
            end if;
            octets(indice_octet) := T_Octet(valeur_courante);
            indice_octet := indice_octet + 1;
            valeur_courante := 0;         
         elsif c in '0'..'9' then
            valeur_courante := valeur_courante * 10  + Character'Pos(C) - Character'Pos('0');
         else
            raise Adresse_IP_Introuvable_Error;
         end if;
      end loop;
      if indice_octet /= 4 then
         raise Adresse_IP_Introuvable_Error;
      end if;
      if valeur_courante > 255 then
         raise Adresse_IP_Introuvable_Error;
      end if;

      for i in 1..4 loop
         adresse_IP := adresse_IP * UN_OCTET + T_Adresse_IP(octets(i));
      end loop;
      return adresse_IP;
   end id_ad_IP;

end Fonctions_globales;