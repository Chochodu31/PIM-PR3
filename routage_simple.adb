with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings; 		use Ada.Strings;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;	use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;		use Ada.Command_Line;
with Fonctions_globales; use Fonctions_globales;
use Fonctions_globales.Adresse_IP_IO;
use Fonctions_globales.LCA_routeur_simple;
with Sda_Exceptions;		use Sda_Exceptions;
with Routeur_exceptions; use Routeur_exceptions;

procedure Routage_simple is

	Cache : Integer;
	Politique: Tab_Politique;
	Statistique : Boolean;
	Table : Unbounded_String;
	Paquet : Unbounded_String;
	Resultat : Unbounded_String;
	Nb_cmd : Integer;
   Tab_routage : T_LCA;
   Entree : File_Type;
	Sortie : File_Type;
   Texte : Unbounded_String;
   Ligne : Integer;
   IP_cmd : Boolean;
   Valeur : T_Case;
   Masque : T_Adresse_IP;
   Int : Unbounded_String;
   Adresse_IP : T_Adresse_IP;
   Association : Integer;
   Arg : Unbounded_String;
	
begin
   -- Comprendre la ligne de commande
	Cache := 10;
	Politique := FIFO;
	Statistique := True;
	Table := To_Unbounded_String("table.txt");
	Paquet := To_Unbounded_String("paquets.txt");
	Resultat := To_Unbounded_String("resultats.txt");

   -- Gérer la ligne de commande
	Nb_cmd := 1;
	while Nb_cmd <= Argument_Count loop
      
      -- Traiter la ligne de commande --------- Impossible de faire des cases avec des String => passage a des elsif
		if Argument(Nb_cmd) = "-c" then
         -- Traiter cas c
         begin
			   Nb_cmd := Nb_cmd + 1;
			   Cache := Integer'Value(Argument(Nb_cmd));
         exception
            when Constraint_Error => 
               Put("Erreur : incompréhension après commande -c");
               raise Commande_Inconnu_Error;
         end;


      elsif Argument (Nb_cmd) = "-p" then 
         -- Traiter cas p
         begin
            Nb_cmd := Nb_cmd + 1;
			   Arg := To_Unbounded_String(Argument(Nb_cmd));
            if To_String(Arg) = "FIFO" then
               Politique := FIFO;
            elsif To_String(Arg) = "LRU" then
               Politique := LRU;
            elsif To_String(Arg) = "LFU" then
               Politique := LFU;
            else
               raise Constraint_Error;
            end if;
         exception
            when Constraint_Error =>
            Put("Erreur : incompréhension après commande -p");
            raise Commande_Inconnu_Error;
         end;
			

      elsif Argument(Nb_cmd) = "-s" then
         Statistique := True;
		
      elsif Argument (Nb_cmd) = "-S" then
         Statistique := False;

      elsif Argument (Nb_cmd) = "-t" then
         -- Traiter cas t
         begin
            Nb_cmd := Nb_cmd + 1;
			   Table := To_Unbounded_String(Argument(Nb_cmd));
         exception
            when Constraint_Error => 
            Put("Erreur : incompréhension après commande -t");
            raise Commande_Inconnu_Error;
         end;
			

      elsif Argument (Nb_cmd) = "-q" then
         -- Traiter cas q 
         begin
            Nb_cmd := Nb_cmd + 1;
			   Paquet := To_Unbounded_String(Argument(Nb_cmd));
         exception
            when Constraint_Error => 
            Put("Erreur : incompréhension après commmande -q");
            raise Commande_Inconnu_Error;
         end;
			

      elsif Argument (Nb_cmd) = "-r" then
         -- Traiter cas r
         begin
            Nb_cmd := Nb_cmd + 1;
            Resultat := To_Unbounded_String(Argument(Nb_cmd));
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


   -- Agir sur la ligne de commande
   if Statistique then
      Put("Rien à afficher car pas encore de cache");
   else
      Null;
   end if;

   -- Créer la table de routage
   Initialiser (Tab_routage);
   table_routage(To_String(Table), Tab_routage);


   -- Mettre en place l'entrée et la sortie
	Create (Sortie, Out_File, To_String (Resultat));
   begin
	   Open (Entree, In_File, To_String (Paquet));
   exception
      when Name_Error =>
         Put("Erreur : " & To_String(Paquet) & " inconnu");
         raise Fichier_Inconnu_Error;
   end;

   begin
      -- Traiter les paquets à router
      while not End_Of_File (Entree) loop
         -- Traiter le paquet à router
         Texte := Get_Line(Entree);
         Ligne := Integer (line(Entree));
         Trim(Texte, both);

         -- Identifier commande ou adresse IP
         IP_cmd := (To_String(Texte)(1) in '0' .. '9');

         if IP_cmd then
            -- Identifier adresse IP
            --  Put(To_String(Texte));
            --  New_Line;
            Adresse_IP := id_ad_IP (To_String(Texte));
            --  Afficher_Ad_IP (Adresse_IP);
            --  New_Line;


            -- Associer adresse IP et Interface
            Masque := 0;
            Association := 0;
            for i in 1..Taille(Tab_routage) loop
               begin
                  Valeur := La_Valeur(Tab_routage, i);
                  --  Put("Adresse IP : ");
                  --  New_Line;
                  --  Put("Masque : ");
                  --  Afficher_Ad_IP (Valeur.Masque);
                  --  New_Line;
                  --  Put("Destination : ");
                  --  Afficher_Ad_IP (Valeur.Destination);
                  --  New_Line;
                  --  Put("Egalite : ");
                  --  if (Adresse_IP and Valeur.Masque) = Valeur.Destination then
                  --     Put("Vrai");
                  --  else
                  --     Put("Faux");
                  --  end if;
                  --  New_Line;
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
               Put(Sortie, " ");
               Put(Sortie, Int);
               New_Line (Sortie);
            end if;
         
         else
            -- Identifier commande
            if Texte = "table" then
               Afficher_Debug_routeur_simpe(Tab_routage);
               
            elsif Texte = "cache" then
               Put("Commande non programmé: affichage Cache");
               
            elsif Texte = "stat" then
               Put("Commande non programmé: Affichafe stat Cache");
               
            elsif Texte = "fin" then
               raise End_Error;
               
            else 
               -- Traiter erreur commande texte
               Put ("Erreur à la ligne : ");
               Put(Ligne);
               raise Commande_Inconnu_Error;
            end if;
         end if;
      end loop;
   exception
      when End_Error => null;
   end;

   -- Fermer les fichiers
   Close (Entree);
	Close (Sortie);
   Detruire(Tab_routage);
end Routage_simple;