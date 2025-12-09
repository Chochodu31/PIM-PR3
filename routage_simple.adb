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
with Routeur_exceptions; use Routeur_exceptions;

procedure Routage_simple is

   type T_Octet is mod 2**8;

  	type T_Adresse_IP is mod 2 ** 32;

	package Octet_IO is new Modular_IO (T_Octet);
	use Octet_IO;

	package Adresse_IP_IO is new Modular_IO (T_Adresse_IP);
	use Adresse_IP_IO;

	UN_OCTET: constant T_Adresse_IP := 2 ** 8;

   type T_Case is record
      Masque : T_Adresse_IP;
      Int : Unbounded_String;
   end record;
   
   package LCA_routeur_simple is new LCA (
      T_Cle => T_Adresse_IP,
      T_Valeur => T_Case
   );
   use LCA_routeur_simple;

   procedure Afficher_Cle_Ad_IP(Cle: in T_Adresse_IP) is
   begin
      Put("Cle");
   end Afficher_Cle_Ad_IP;

   procedure Afficher_Donnee_Enregistrement(Val: in T_Case) is
   begin
      Put("Enregistrement");
   end Afficher_Donnee_Enregistrement;

   procedure Afficher_Debug_routeur_simpe is new Afficher_Debug(
      Afficher_Cle => Afficher_Cle_Ad_IP,
      Afficher_Donnee => Afficher_Donnee_Enregistrement
   );

   procedure table_routage(table : in String; LCA: in out T_LCA) is
      Enregistrement : T_Case;
   begin
      Enregistrement.Masque := -1;
      Enregistrement.Int := To_Unbounded_String("Salut");
      Enregistrer (LCA, 1, Enregistrement);
   end table_routage;

   function id_ad_IP (ad_IP : in String) return T_Adresse_IP is
      t : T_Adresse_IP;
   begin
      t := -1;
      return t;
   end id_ad_IP;

   -- Define the array type
   type T_M is array (1 .. 5) of T_Adresse_IP;

   -- Declare an object of that type
   M : constant T_M := (
      5 => (255 * UN_OCTET**3) + (255 * UN_OCTET**2) + (255 * UN_OCTET**1) + 255,
      4 => (255 * UN_OCTET**3) + (255 * UN_OCTET**2) + (255 * UN_OCTET**1),
      3 => (255 * UN_OCTET**3) + (255 * UN_OCTET**2),
      2 => (255 * UN_OCTET**3),
      1 => 0
   );

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
			Nb_cmd := Nb_cmd + 1;
			Cache := Traiter_c(Argument(Nb_cmd));

      elsif Argument (Nb_cmd) = "-p" then 
         -- Traiter cas p
			Nb_cmd := Nb_cmd + 1;
			Politique := Traiter_p(Argument(Nb_cmd));

      elsif Argument(Nb_cmd) = "-s" then
         Statistique := True;
		
      elsif Argument (Nb_cmd) = "-S" then
         Statistique := False;

      elsif Argument (Nb_cmd) = "-t" then
         -- Traiter cas t
			Nb_cmd := Nb_cmd + 1;
			Table := To_Unbounded_String(Traiter_t(Argument(Nb_cmd)));

      elsif Argument (Nb_cmd) = "-q" then
         -- Traiter cas q 
			Nb_cmd := Nb_cmd + 1;
			Paquet := To_Unbounded_String(Traiter_q(Argument(Nb_cmd)));

      elsif Argument (Nb_cmd) = "-r" then
         -- Traiter cas r
         Nb_cmd := Nb_cmd + 1;
         Resultat := To_Unbounded_String(Traiter_r(Argument(Nb_cmd)));

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
            Adresse_IP := id_ad_IP (To_String(Texte));

            -- Associer adresse IP et Interface
            for i in 1..5 loop
               begin
                  Valeur := La_Valeur(Tab_routage, Adresse_IP AND M(i));
                  Masque := Valeur.Masque;
                  if not (Masque = M(i)) then
                     raise Cle_Absente_Error;
                  else
                     Null;
                  end if;
                  Int := Valeur.Int;
               exception
                  when Cle_Absente_Error => Null;
               end;
            end loop;

            Put(Sortie, Adresse_IP);
            Put(Sortie, " ");
            Put(Sortie, Int);

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