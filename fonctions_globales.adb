with Routeur_exceptions; use Routeur_exceptions;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;

package body Fonctions_globales is
   type T_Octet is mod 2 ** 8;
   UN_OCTET: constant T_Adresse_IP := 2 ** 8;

   procedure Afficher_Ad_IP(M1 : in T_Adresse_IP) is
   begin
      Put (Natural ((M1 / UN_OCTET ** 3) mod UN_OCTET), 1); 
      Put (".");
      Put (Natural ((M1 / UN_OCTET ** 2) mod UN_OCTET), 1); 
      Put (".");
      Put (Natural ((M1 / UN_OCTET ** 1) mod UN_OCTET), 1); 
      Put (".");
      Put (Natural  (M1 mod UN_OCTET), 1);
   end Afficher_Ad_IP;

   procedure Ecrire_Ad_IP(Sortie : in out File_Type; M1 : in T_Adresse_IP) is
   begin
      Put (Sortie, Natural ((M1 / UN_OCTET ** 3) mod UN_OCTET), 1); 
      Put (Sortie, ".");
      Put (Sortie, Natural ((M1 / UN_OCTET ** 2) mod UN_OCTET), 1); 
      Put (sortie, ".");
      Put (Sortie, Natural ((M1 / UN_OCTET ** 1) mod UN_OCTET), 1); 
      Put (Sortie, ".");
      Put (Sortie, Natural  (M1 mod UN_OCTET), 1);
   end Ecrire_Ad_IP;
   

   procedure Afficher_Cle_Ad_IP(Cle: in Integer) is
   begin
      Put(Cle, 1);
   end Afficher_Cle_Ad_IP;


   procedure Afficher_Donnee_Enregistrement(Val: in T_Case) is
   begin
      Put("(");
      Afficher_Ad_IP(Val.Destination);
      Put(", ");
      Afficher_Ad_IP(Val.Masque);
      Put(", ");
      Put(To_String(Val.Int));
      Put(")");
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
         --  Put("Passage ici");
         --  New_Line;
         -- Comprendre la ligne
         Texte := To_Unbounded_String(Get_Line(Entree));

         -- Décomposer en trois éléments         
         Colonne := 1;
         Compteur_Espace := False;
         Tab (1) := To_Unbounded_String ("");
         Tab (2) := To_Unbounded_String ("");
         Tab (3) := To_Unbounded_String ("");
         for Compteur in 1..length(Texte) loop
            if To_String(Texte)(Compteur) = ' ' and Compteur_Espace then
               null;
            elsif To_String(Texte)(Compteur)= ' ' and not Compteur_Espace then
               compteur_espace := True;
               colonne := colonne +1;
            else 
               compteur_espace := False;
               Tab(colonne) := Tab(colonne) & To_String(Texte)(compteur);                      
            end if;
         end loop;
         --  Put("Destination : ");
         --  Put(To_String(Tab(1)));
         --  New_Line;
         --  Put("Masque : ");
         --  Put(To_String(Tab(2)));
         --  New_Line;
         --  Put("Interface : ");
         --  Put(To_String(Tab(3)));
         --  New_Line;

         -- Ajouter à Tab_Routage
         --  Put("Passage à Masque : ");
         --  New_Line;
         Masque := id_ad_IP(To_String(Tab(2)));
         --  Put("Passage à Destination : ");
         --  New_Line;
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
         --  Put("Character C : ");
         --  Put(c);
         --  New_Line;
         --  Put(octets);
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
      octets(4) := T_Octet(valeur_courante);

      if indice_octet /= 4 then
         raise Adresse_IP_Introuvable_Error;
      end if;
      if valeur_courante > 255 then
         raise Adresse_IP_Introuvable_Error;
      end if;

      --  Put("1 terme de octets : ");
      --  Afficher_Ad_IP(T_Adresse_IP(octets(1)));
      --  New_Line;
      --  Put("2 terme de octets : ");
      --  Afficher_Ad_IP(T_Adresse_IP(octets(2)));
      --  New_Line;
      --  Put("3 terme de octets : ");
      --  Afficher_Ad_IP(T_Adresse_IP(octets(3)));
      --  New_Line;
      --  Put("Dernier terme de octets : ");
      --  Afficher_Ad_IP(T_Adresse_IP(octets(4)));

      for i in 1..4 loop
         adresse_IP := adresse_IP * UN_OCTET + T_Adresse_IP(octets(i));
         --  Afficher_Ad_IP(adresse_IP);
         --  New_Line;
      end loop;
      return adresse_IP;
   end id_ad_IP;

end Fonctions_globales;