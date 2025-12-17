with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Fonctions_globales;   use Fonctions_globales;
use Fonctions_globales.LCA_routeur_simple;
with Routeur_exceptions;   use Routeur_exceptions;
with ada.Integer_Text_IO;  use ada.Integer_Text_IO;

procedure Routage_simple is

	Cache_Taille : Integer;  -- Taille du cache
	Politique: Tab_Politique;  -- Politique de traitement de cache
	Statistique : Boolean;  -- Affichage des statistiques
	Table : Unbounded_String;  -- Nom du fichier contenant les éléments de la table de routage
	Paquet : Unbounded_String; -- Nom du fichier contenant les éléments à router
	Resultat : Unbounded_String;  -- Nom du fichier dans lequel écrire les résultats
   Tab_routage : T_LCA; -- Table de routage
   Entree : File_Type;  -- Fichier d'entrée
	Sortie : File_Type;  -- Fichier de sortie
	
begin
   -- Comprendre la ligne de commande
	Gerer_commandes (Cache_Taille, Politique, Statistique, Table, Paquet, Resultat);


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
      Traiter_les_paquets (Entree, Sortie, Tab_routage);
      --  while not End_Of_File (Entree) loop
      --     -- Traiter le paquet à router
      --     Texte := Get_Line (Entree);
      --     Ligne := Integer (line(Entree));
      --     Trim (Texte, both);

      --     -- Identifier commande ou adresse IP
      --     IP_cmd := (To_String(Texte)(1) in '0' .. '9');


      --     if IP_cmd then
      --        -- Identifier adresse IP
      --        Adresse_IP := Id_ad_IP (To_String(Texte));

      --        -- Associer adresse IP et Interface
      --        Int := Association_ad_des (Tab_Routage, Adresse_IP);
      --        Ecrire (Sortie, Adresse_IP, To_String(Int));
      --     else
      --        -- Identifier commande
      --        Identifier_commande (To_String(Texte), Ligne, Tab_routage);

      --     end if;
      --  end loop;
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
   Put(Cache_Taille, 2);

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