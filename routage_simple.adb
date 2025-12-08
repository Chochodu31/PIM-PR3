with Ada.Text_IO;		use Ada.Texte_IO;
with Ada.Strings; 		use Ada.Strings;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;	use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Exceptions;		use Ada.Exceptions;
with Sda;   use Sda;

procedure Routeur_simple is
	Cache : Integer;
	type Tab_Politique is (FIFO, LRU, LFU);
	Politique: Tab_Politique;
	Statistique : Boolean;
	Table : String;
	Paquet : String;
	Resultat : String;
	Nb_cmd : Integer;
	
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

			when others => Traiter_erreur;
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
end Routeur_simple;


function Traiter_c(Arg : in String; Cache : in out String) return String is
begin
	Cache := Integer(String);
exception
	when Constraint_Error => Put("Erreur : incompréhension après commande -c");
end Traiter_c;


function Traiter_p (Arg : in String; Politique : in out Tab_Politique) return Tab_Politique is
begin
   Politique := Arg;
exception
   when Constraint_Error => Put("Erreur : incompréhension après commande -p");
end Traiter_p;


function Traiter_t (Arg : in String; Table : in out String) return String is
begin
   Table := Arg;
exception
   when Constraint_Error => Put("Erreur : incompréhension après commande -t");
end Traiter_t;


function Traiter_q (Arg : in String; Paquet : in out String) return String is
begin
   Paquet := Arg;
exception
   when Constraint_Error => Put("Erreur : incompréhension après commmande -q");
end Traiter_q;


function Traiter_r (Arg: in String; Resultat : in out String) return String is
begin
   Resultat := Arg;
exception
   when Constraint_Error => Put("Erreur incompréhension après commande -r");
end Traiter_r;