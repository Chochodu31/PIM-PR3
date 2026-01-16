with Routeur_exceptions; use Routeur_exceptions;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Strings; 		use Ada.Strings;


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
      type Tab_Octets is array (1..4) of T_Octet;
      adresse_IP : T_Adresse_IP;
      Octets : Tab_Octets := (0, 0, 0, 0);
      indice_octet : Integer := 1;
      valeur_courante : Integer := 0;
      caractere : Character;
      
      function valeur_numerique(c : Character) return Integer is
      begin
         return Character'Pos(c) - Character'Pos('0');
      end valeur_numerique;
      
   begin
      if Texte'Length = 0 then
         raise Adresse_IP_Introuvable_Error;
      else
         null;
      end if;
      
      for i in 1..Texte'Length loop
         caractere := Texte(i);
         if caractere = '.' then
            if indice_octet > 3 then
               raise Adresse_IP_Introuvable_Error; 
            elsif valeur_courante < 0 or valeur_courante > 255 then
               raise Adresse_IP_Introuvable_Error;
            else
               null;
            end if;
            
            octets(indice_octet) := T_Octet(valeur_courante);
            indice_octet := indice_octet + 1;
            valeur_courante := 0;
            
            if i = 1 or Texte(i-1) = '.' then
               raise Adresse_IP_Introuvable_Error;
            else
               null;
            end if;
            
         elsif caractere in '0'..'9' then
            valeur_courante := valeur_courante * 10 + valeur_numerique(caractere);
            
            if valeur_courante > 255 then
               raise Adresse_IP_Introuvable_Error;
            else
               null;
            end if;
            
         else
            raise Adresse_IP_Introuvable_Error;
         end if;
      end loop;
      
      if indice_octet /= 4 then
         raise Adresse_IP_Introuvable_Error;
      else
         null;
      end if;
      
      if valeur_courante < 0 or valeur_courante > 255 then
         raise Adresse_IP_Introuvable_Error;
      else
         null;
      end if;
      
      octets(4) := T_Octet(valeur_courante);

      if Texte(Texte'Last) = '.' then
         raise Adresse_IP_Introuvable_Error;
      else
         null;
      end if;
      
      adresse_IP := 0;
      for i in 1..4 loop
         adresse_IP := adresse_IP * UN_OCTET + T_Adresse_IP(octets(i));
      end loop;
      return adresse_IP;
      
   exception
      when others =>
         raise Adresse_IP_Introuvable_Error;
   end Id_ad_IP;


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
      begin
         Open (Entree, In_File, table);
      exception
         when Name_Error =>
            raise Fichier_Inconnu_Error;
      end;

      while not End_Of_File (Entree) loop
         Texte := To_Unbounded_String (Get_Line (Entree));

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
               colonne := colonne + 1;
            else 
               compteur_espace := False;
               Tab (colonne) := Tab (colonne) & To_String(Texte)(compteur);                      
            end if;
         end loop;

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

      Nb_cmd := 1;
      while Nb_cmd <= Argument_Count loop
         
         if Argument (Nb_cmd) = "-c" then
            begin
               Nb_cmd := Nb_cmd + 1;
               Cache := Integer'Value (Argument (Nb_cmd));
               if Cache <= 0 then
                  Put_Line("Erreur : la taille du cache doit être positive");
                  raise Commande_Inconnu_Error;
               else
                  null;
               end if;
            exception
               when Constraint_Error => 
                  Put_Line ("Erreur : incompréhension après commande -c");
                  raise Commande_Inconnu_Error;
            end;


         elsif Argument (Nb_cmd) = "-p" then 
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
               Put_Line("Erreur : incompréhension après commande -p");
               raise Commande_Inconnu_Error;
            end;
            

         elsif Argument (Nb_cmd) = "-s" then
            Statistique := True;
         
         elsif Argument (Nb_cmd) = "-S" then
            Statistique := False;

         elsif Argument (Nb_cmd) = "-t" then
            begin
               Nb_cmd := Nb_cmd + 1;
               Table := To_Unbounded_String (Argument (Nb_cmd));
            exception
               when Constraint_Error => 
               Put_Line("Erreur : incompréhension après commande -t");
               raise Commande_Inconnu_Error;
            end;
            

         elsif Argument (Nb_cmd) = "-q" then
            begin
               Nb_cmd := Nb_cmd + 1;
               Paquet := To_Unbounded_String (Argument (Nb_cmd));
            exception
               when Constraint_Error => 
               Put_Line("Erreur : incompréhension après commmande -q");
               raise Commande_Inconnu_Error;
            end;
            

         elsif Argument (Nb_cmd) = "-r" then
            begin
               Nb_cmd := Nb_cmd + 1;
               Resultat := To_Unbounded_String (Argument (Nb_cmd));
            exception
               when Constraint_Error => 
               Put_Line("Erreur incompréhension après commande -r");
               raise Commande_Inconnu_Error;
            end;

         else
            Put ("Erreur à l'argument : ");
            Put (Nb_cmd);
            New_Line;
            raise Commande_Inconnu_Error;
         end if;

         Nb_cmd := Nb_cmd + 1;
      end loop;
   end Gerer_commandes;


   procedure Ouvrir(Paquet : in String; Entree : in out File_Type) is
   begin
      begin
	      Open (Entree, In_File, Paquet);
      exception
         when Name_Error =>
            Put_Line("Erreur : " & Paquet & " inconnu");
            raise Fichier_Inconnu_Error;
      end;
   end Ouvrir;


   function Trouver_Meilleure_Route (Tab_Routage : in T_LCA; Adresse_IP : in T_Adresse_IP) return T_Case is
      Meilleure_Route : T_Case;
      Meilleur_Masque : T_Adresse_IP := 0;
      Trouvee : Boolean := False;
      
      -- Paramètre Cle non utilisé, on le renomme pour éviter le warning
      procedure Traiter_Route(Unused_Cle : in Integer; Valeur : in T_Case) is
         pragma Unreferenced (Unused_Cle); -- Indique explicitement que le paramètre n'est pas utilisé
      begin
         if (Adresse_IP and Valeur.Masque) = Valeur.Destination then
            if Valeur.Masque >= Meilleur_Masque then
               Meilleur_Masque := Valeur.Masque;
               Meilleure_Route := Valeur;
               Trouvee := True;
            else
               null;
            end if;
         else
            null;
         end if;
      end Traiter_Route;
      
      procedure Parcourir_Routes is new Faire_Pour_Chaque(Traiter => Traiter_Route);
      
   begin
      Parcourir_Routes(Tab_Routage);
      
      if not Trouvee then
         raise Adresse_IP_Introuvable_Error;
      else
         null;
      end if;
      
      return Meilleure_Route;
   end Trouver_Meilleure_Route;


   procedure Ecrire (Sortie : in out File_Type; Adresse_IP : in T_Adresse_IP; Int : in String) is
   begin
      Ecrire_Ad_IP (Sortie, Adresse_IP);
      Put (Sortie, " ");
      Put (Sortie, Int);
      New_Line (Sortie);
   end Ecrire;


   procedure Identifier_commande (Texte : in String; Ligne : in Integer; Tab_routage : in T_LCA) is
   begin
      if Texte = "table" then
         Afficher_table_routage (Tab_routage);
               
      elsif Texte = "cache" then
         Put_Line ("Commande non programmé: affichage Cache");
               
      elsif Texte = "stat" then
         Put_Line ("Commande non programmé: Affichafe stat Cache");
               
      elsif Texte = "fin" then
         raise End_Error;
               
      else 
         Put ("Erreur à la ligne : ");
         Put (Ligne);
         New_Line;
         raise Commande_Inconnu_Error;
      end if;
   end Identifier_commande;


   procedure Traiter_les_paquets(Entree : in File_Type; Sortie : in out File_Type; Tab_routage : in T_LCA) is
      Texte : Unbounded_String;
      Ligne : Integer := 0;
      IP_cmd : Boolean;
      Adresse_IP : T_Adresse_IP;
      Route_Trouvee : T_Case;

   begin
      begin 
         while not End_Of_File (Entree) loop
            
            Texte := To_Unbounded_String(Get_Line (Entree));
            Ligne := Ligne + 1;
            Trim (Texte, both);

            IP_cmd := (Length(Texte) > 0 and then To_String(Texte)(1) in '0' .. '9');

            if IP_cmd then
               Adresse_IP := Id_ad_IP (To_String(Texte));

               Route_Trouvee := Trouver_Meilleure_Route (Tab_Routage, Adresse_IP);
               
               Ecrire (Sortie, Adresse_IP, To_String(Route_Trouvee.Int));
               
            else
               Identifier_commande (To_String(Texte), Ligne, Tab_routage);
            end if;
         end loop;
      exception
         when End_Error => 
            null;
      end;
   end Traiter_les_paquets;


end Fonctions_globales;