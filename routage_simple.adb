with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings; 		use Ada.Strings;
with Ada.Integer_Text_IO; 	use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;	use Ada.Text_IO.Unbounded_IO;
with Fonctions_globales; use Fonctions_globales;
use Fonctions_globales.LCA_routeur_simple;
with Routeur_exceptions; use Routeur_exceptions;

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
   Valeur : T_Case;
   Masque : T_Adresse_IP;
   Int : Unbounded_String;
   Adresse_IP : T_Adresse_IP;
   Association : Integer;
	
begin
   -- Comprendre la ligne de commande
	Gerer_commandes (Cache, Politique, Statistique, Table, Paquet, Resultat);


   -- Agir sur la ligne de commande
   -- Agir selon Statistique
   if Statistique then
      Put("Rien à afficher car pas encore de cache");
   else
      Null;
   end if;

   -- Agir selon Cache
   if Cache = 0 then
      Null;
   else
      Put("Pas pris en compte : ici on est à routage simple");
   end if;

   -- Agir selon Politique
   if Politique in Tab_Politique then
      Put("La politique n'est pas pris en charge car il n'y a pas de cache");
   else
      null;
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
         Texte := Get_Line (Entree);
         Ligne := Integer (line(Entree));
         Trim(Texte, both);

         -- Identifier commande ou adresse IP
         IP_cmd := (To_String(Texte)(1) in '0' .. '9');

         if IP_cmd then
            -- Identifier adresse IP
            Adresse_IP := id_ad_IP (To_String(Texte));


            -- Associer adresse IP et Interface
            association_ad_des(Tab_Routage,  Sortie, Adresse_IP);

            --  Masque := 0;
            --  Association := 0;
            --  for i in 1..Taille(Tab_routage) loop
            --     begin
            --        Valeur := La_Valeur(Tab_routage, i);
            --        if ((Adresse_IP and Valeur.Masque) = Valeur.Destination) and (Masque <= Valeur.Masque) then
            --           Association := Association + 1;
            --           Masque := Valeur.Masque;
            --           Int := Valeur.Int;
            --        else
            --           null;
            --        end if;
            --     exception
            --        when Cle_Absente_Error => Null;
            --     end;
            --  end loop;
            --  if Association = 0 then
            --     raise Adresse_IP_Introuvable_Error;
            --  else
            --     Ecrire_Ad_IP (Sortie, Adresse_IP);
            --     Put(Sortie, " ");
            --     Put(Sortie, Int);
            --     New_Line (Sortie);
            --  end if;
         
         else
            -- Identifier commande
            identifier_commande (To_String(Texte), Ligne, Tab_routage);

            --  if Texte = "table" then
            --     Afficher_Debug_routeur_simpe(Tab_routage);
               
            --  elsif Texte = "cache" then
            --     Put("Commande non programmé: affichage Cache");
               
            --  elsif Texte = "stat" then
            --     Put("Commande non programmé: Affichafe stat Cache");
               
            --  elsif Texte = "fin" then
            --     raise End_Error;
               
            --  else 
            --     -- Traiter erreur commande texte
            --     Put ("Erreur à la ligne : ");
            --     Put(Ligne);
            --     raise Commande_Inconnu_Error;
         --     end if;
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