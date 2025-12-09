with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings; 		use Ada.Strings;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;	use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Exceptions;		use Ada.Exceptions;
with Fonctions_globales; use Fonctions_globales;
with LCA;
with Sda_Exceptions;		use Sda_Exceptions;

procedure Routage_simple is

   function table_routage(table : in out String) return T_LCA is
      Liste : T_LCA;
      Enregistrement  :T_Case;
   begin
      Enregistrement.Masque := -1;
      Enregistrement.Interface := "Salut";
      Initialiser (Liste);
      Ajouter (Liste, 1, Enregistrement);
      return Liste;
   end table_routage;

	Cache : Integer;
	Politique: Tab_Politique;
	Statistique : Boolean;
	Table : String;
	Paquet : String;
	Resultat : String;
	Nb_cmd : Integer;
   Tab_routage : T_LCA;
	Nom_Sortie : Unbounded_String;
	Nom_Entree : Unbounded_String;
   Entree : File_Type;
	Sortie : File_Type;
   Texte : String;
   Ligne : Integer;
   IP_cmd : Boolean;
   Valeur : T_Case;
   Masque : T_Adresse_IP;
	
begin
   -- Comprendre la ligne de commande
	Cache := 10;
	Politique := FIFO;
	Statistique := True;
	Table := "table.txt";
	Paquet := "paquets.txt";
	Resultat := "resultats.txt";

   -- Gérer la ligne de commande
	Nb_cmd := 1;
	while Nb_cmd <= Argument_Count loop
      
      -- Traiter la ligne de commande
		case Argument(Nb_cmd) is
         when "-c" => 

            -- Traiter cas c
				Nb_cmd := Nb_cmd + 1;
				Cache := Traiter_c(Argument(Nb_cmd), Cache);

			when "-p" =>  
            -- Traiter cas p
				Nb_cmd := Nb_cmd + 1;
				Politique := Traiter_p(Argument(Nb_cmd), Politique);

			when "-s" => Statistique := True;
			when "-S" => Statistique := False;

			when "-t" =>
            -- Traiter cas t
				Nb_cmd := Nb_cmd + 1;
				Table := Traiter_t(Argument(Nb_cmd), Table);

			when "-q" =>
            -- Traiter cas q 
				Nb_cmd := Nb_cmd + 1;
				Paquet := Traiter_q(Argument(Nb_cmd), Paquet);

         when "-r" =>
            -- Traiter cas r
            Nb_cmd := Nb_cmd + 1;
            Resultat := Traiter_r(Argument(Nb_cmd), Resultat);

			when others =>
            Put ("Erreur à l'argument : " & Nb_cmd);
            raise Commande_Inconnu_Error;
		end case;

		Nb_cmd := Nb_cmd + 1;
	end loop;


   -- Agir sur la ligne de commande
   if Statistique then
      Put("Rien à afficher car pas encore de cache");
   else
      Null;
   end if;

   -- Créer la table de routage
   Tab_routage := table_routage(Table);

   -- Mettre en place l'entrée et la sortie
   Nom_Entree := To_Unbounded_String (Paquet);
	Nom_Sortie := To_Unbounded_String (Resultat);
	Create (Sortie, Out_File, To_String (Nom_Sortie));
   begin
	   Open (Entree, In_File, To_String (Nom_Entree));
   exception
      when Name_Error =>
         Put("Erreur : " & String(Nom_Entree) & " inconnu");
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
         IP_cmd := (Texte(1) in '0' .. '9');

         if IP_cmd then
            -- Identifier adresse IP
            id_ad_IP (Texte);

            -- Associer adresse IP et Interface
            for i in 1..5 loop
               begin
                  Valeur := La_Valeur(Tab_routage, Adresse_IP AND M(i));
                  Masque := Valeur.Masque;
                  if not Masque = M(i) then
                     raise Cle_Absente_Error;
                  else
                     Null;
                  end if;
                  Interface := Valeur.Interface;
               exception
                  when Cle_Absente_Error => Null;
               end;
            end loop;

            Put(Sortie, Adresse_IP & " " & Interface);
         else
            -- Identifier commande
            case Texte is 
               when "table" => Afficher_Debug(Tab_routage);
               when "cache" => Put("Commande non programmé: affichage Cache");
               when "stat" => Put("Commande non programmé: Affichafe stat Cache");
               when "fin" => raise End_Error;
               when others => 
                  -- Traiter erreur commande texte
                  Put ("Erreur à la ligne : " & Ligne);
                  raise Commande_Inconnu_Error;
            end case;
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