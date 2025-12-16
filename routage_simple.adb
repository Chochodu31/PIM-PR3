with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings; 		use Ada.Strings;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;	use Ada.Text_IO.Unbounded_IO;
with Fonctions_globales; use Fonctions_globales;
use Fonctions_globales.LCA_routeur_simple;
with Routeur_exceptions; use Routeur_exceptions;
with ada.Integer_Text_IO; use ada.Integer_Text_IO;

procedure Routage_simple is

	Cache : Integer;
	Politique: Tab_Politique;
	Statistique : Boolean;
	Table : Unbounded_String;
	Paquet : Unbounded_String;
	Resultat : Unbounded_String;
   Tab_routage : T_LCA;
   Entree : File_Type;
	Sortie : File_Type;
   Texte : Unbounded_String;
   Ligne : Integer;
   IP_cmd : Boolean;
   Adresse_IP : T_Adresse_IP;
	
begin
   -- Comprendre la ligne de commande
	Gerer_commandes (Cache, Politique, Statistique, Table, Paquet, Resultat);


   -- Créer la table de routage
   Initialiser (Tab_routage);
   Table_routage(To_String(Table), Tab_routage);


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
         Texte := Get_Line (Entree);
         Ligne := Integer (line(Entree));
         Trim (Texte, both);

         -- Identifier commande ou adresse IP
         IP_cmd := (To_String(Texte)(1) in '0' .. '9');

         if IP_cmd then
            -- Identifier adresse IP
            Adresse_IP := Id_ad_IP (To_String(Texte));


            -- Associer adresse IP et Interface
            Association_ad_des(Tab_Routage,  Sortie, Adresse_IP);
         
         else
            -- Identifier commande
            Identifier_commande (To_String(Texte), Ligne, Tab_routage);

         end if;
      end loop;
   exception
      when End_Error => null;
   end;

   -- Fermer les fichiers
   Close (Entree);
	Close (Sortie);
   Detruire(Tab_routage);


   -- Agir sur la ligne de commande
   -- Agir selon Statistique
   if Statistique then
      New_Line;
      Put("Rien à afficher car pas encore de cache");
   else
      Null;
   end if;

   -- Agir selon Cache
   New_Line;
   Put("Taille cache : ");
   Put(Cache, 2);

   -- Agir selon Politique
   New_Line;
   if Politique = FIFO then
      Put("La politique choisi : FIFO");
   elsif Politique = LRU then
      Put("Politique choisi : LRU");
   else
      Put("Politique choisi : LFU");
   end if;
end Routage_simple;