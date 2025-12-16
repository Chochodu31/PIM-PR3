with Routeur_exceptions; use Routeur_exceptions;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;
with Ada.Command_Line;		use Ada.Command_Line;
with Sda_Exceptions;		use Sda_Exceptions;

package body Fonctions_globales is

   type T_Octet is mod 2 ** 8;

   UN_OCTET: constant T_Adresse_IP := 2 ** 8;
   
   
   -- Afficher l'adresse IP.
   -- Exemple d'affichage : 
   -- 32.248.90.14
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


   procedure Afficher_Cle_Integer (Cle: in Integer) is
   begin
      Put(Cle, 1);
   end Afficher_Cle_Integer;


   procedure Afficher_Donnee_Case (Val: in T_Case) is
   begin
      Put ("(");
      Afficher_Ad_IP (Val.Destination);
      Put (", ");
      Afficher_Ad_IP (Val.Masque);
      Put (", ");
      Put (To_String (Val.Int));
      Put (")");
   end Afficher_Donnee_Case;


   -- Ecrire dans le Fichier Sortie l'adresse M1
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


   function Id_ad_IP (Texte : in String) return T_Adresse_IP is
      indice_octet : Integer := 1;
      valeur_courante : Integer := 0;
      c : Character;
      type T_Tableau_Octets is array (1..4) of T_Octet;
      octets : T_Tableau_Octets;
      adresse_IP : T_Adresse_IP := 0;
   begin
      for i in 1..Length (To_Unbounded_String (Texte)) loop
         c := Texte (i);
         if c = '.' then
            if indice_octet > 3 then
               raise Adresse_IP_Introuvable_Error;
            end if;   
            if valeur_courante > 255 then
               raise Adresse_IP_Introuvable_Error;
            end if;
            octets (indice_octet) := T_Octet (valeur_courante);
            indice_octet := indice_octet + 1;
            valeur_courante := 0;         
         elsif c in '0'..'9' then
            valeur_courante := valeur_courante * 10  + Character'Pos (C) - Character'Pos ('0');
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

      for i in 1..4 loop
         adresse_IP := adresse_IP * UN_OCTET + T_Adresse_IP (octets (i));
      end loop;
      return adresse_IP;
   end id_ad_IP;


   procedure Table_routage (Table: in String; Tab_routage : in out T_LCA) is
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

         -- Comprendre la ligne
         Texte := To_Unbounded_String (Get_Line (Entree));

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
               Tab (colonne) := Tab (colonne) & To_String(Texte)(compteur);                      
            end if;
         end loop;

         -- Ajouter à Tab_Routage
         Masque := Id_ad_IP (To_String (Tab(2)));
         Destination := Id_ad_IP(To_String(Tab(1)));
         Int := Tab(3);
         Enregistrement.Masque := Masque;
         Enregistrement.Destination:= Destination;
         Enregistrement.Int := Int;
         Taille_Var := Taille (Tab_routage);
         Enregistrer (tab_routage, Taille_Var + 1, Enregistrement);
      end loop;
      
      Close (Entree);
   end table_routage;


   procedure Gerer_commandes (Cache : out Integer; 
                              Politique : out Tab_Politique; 
                              Statistique : out Boolean; 
                              Table : out Unbounded_String;
                              Paquet : out Unbounded_String;
                              Resultat : out Unbounded_String ) is
      Nb_cmd : Integer;
      Arg : Unbounded_String;
   begin
      Cache := 10;
      Politique := FIFO;
      Statistique := True;
      Table := To_Unbounded_String ("table.txt");
      Paquet := To_Unbounded_String ("paquets.txt");
      Resultat := To_Unbounded_String ("resultats.txt");

      -- Gérer la ligne de commande
      Nb_cmd := 1;
      while Nb_cmd <= Argument_Count loop
         
         -- Traiter la ligne de commande
         if Argument (Nb_cmd) = "-c" then
            -- Traiter cas c
            begin
               Nb_cmd := Nb_cmd + 1;
               Cache := Integer'Value (Argument (Nb_cmd));
            exception
               when Constraint_Error => 
                  Put ("Erreur : incompréhension après commande -c");
                  raise Commande_Inconnu_Error;
            end;


         elsif Argument (Nb_cmd) = "-p" then 
            -- Traiter cas p
            begin
               Nb_cmd := Nb_cmd + 1;
               Arg := To_Unbounded_String (Argument (Nb_cmd));
               if To_String (Arg) = "FIFO" then
                  Politique := FIFO;
               elsif To_String (Arg) = "LRU" then
                  Politique := LRU;
               elsif To_String (Arg) = "LFU" then
                  Politique := LFU;
               else
                  raise Constraint_Error;
               end if;
            exception
               when Constraint_Error =>
               Put("Erreur : incompréhension après commande -p");
               raise Commande_Inconnu_Error;
            end;
            

         elsif Argument (Nb_cmd) = "-s" then
            Statistique := True;
         
         elsif Argument (Nb_cmd) = "-S" then
            Statistique := False;

         elsif Argument (Nb_cmd) = "-t" then
            -- Traiter cas t
            begin
               Nb_cmd := Nb_cmd + 1;
               Table := To_Unbounded_String (Argument (Nb_cmd));
            exception
               when Constraint_Error => 
               Put("Erreur : incompréhension après commande -t");
               raise Commande_Inconnu_Error;
            end;
            

         elsif Argument (Nb_cmd) = "-q" then
            -- Traiter cas q 
            begin
               Nb_cmd := Nb_cmd + 1;
               Paquet := To_Unbounded_String (Argument (Nb_cmd));
            exception
               when Constraint_Error => 
               Put("Erreur : incompréhension après commmande -q");
               raise Commande_Inconnu_Error;
            end;
            

         elsif Argument (Nb_cmd) = "-r" then
            -- Traiter cas r
            begin
               Nb_cmd := Nb_cmd + 1;
               Resultat := To_Unbounded_String (Argument (Nb_cmd));
            exception
               when Constraint_Error => 
               Put("Erreur incompréhension après commande -r");
               raise Commande_Inconnu_Error;
            end;

         else
            Put ("Erreur à l'argument : ");
            Put (Nb_cmd);
            raise Commande_Inconnu_Error;
         end if;

         Nb_cmd := Nb_cmd + 1;
      end loop;
   end Gerer_commandes;


   procedure association_ad_des (Tab_Routage : in T_LCA; Sortie : in out File_Type; Adresse_IP : in T_Adresse_IP) is
      Masque : T_Adresse_IP;
      Association : Integer;
      Valeur : T_Case;
      Int : Unbounded_String;
   begin
      Masque := 0;
      Association := 0;
      for i in 1..Taille (Tab_routage) loop
         begin
            Valeur := La_Valeur (Tab_routage, i);
            if ((Adresse_IP and Valeur.Masque) = Valeur.Destination) and (Masque <= Valeur.Masque) then
               Association := Association + 1;
               Masque := Valeur.Masque;
               Int := Valeur.Int;
            else
               null;
            end if;
         exception
            when Cle_Absente_Error => Null;
         end;
      end loop;
      if Association = 0 then
         raise Adresse_IP_Introuvable_Error;
      else
         Ecrire_Ad_IP (Sortie, Adresse_IP);
         Put (Sortie, " ");
         Put (Sortie, To_String (Int));
         New_Line (Sortie);
      end if;
   end association_ad_des;



   procedure Identifier_commande (Texte : in String; Ligne : in Integer; Tab_routage : in T_LCA) is
   begin
      if Texte = "table" then
         Afficher_table_routage (Tab_routage);
               
      elsif Texte = "cache" then
         Put ("Commande non programmé: affichage Cache");
               
      elsif Texte = "stat" then
         Put ("Commande non programmé: Affichafe stat Cache");
               
      elsif Texte = "fin" then
         raise End_Error;
               
      else 
         -- Traiter erreur commande texte
         Put ("Erreur à la ligne : ");
         Put (Ligne);
         raise Commande_Inconnu_Error;
      end if;
   end Identifier_commande;

end Fonctions_globales;