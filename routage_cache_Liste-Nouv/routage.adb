with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Strings.Unbounded; 	use Ada.Strings.Unbounded;
with Fonctions_globales;   use Fonctions_globales;
use Fonctions_globales.Routeur_cache;
with ada.Integer_Text_IO;  use ada.Integer_Text_IO;
with ada.Float_Text_IO; use ada.Float_Text_IO;

procedure Routage is

	Cache_Taille : Integer;  -- Taille du cache
	Politique: Tab_Politique;  -- Politique de traitement de cache
	Statistique : Boolean;  -- Affichage des statistiques
	Table : Unbounded_String;  -- Nom du fichier contenant les éléments de la table de routage
	Paquet : Unbounded_String; -- Nom du fichier contenant les éléments à router
	Resultat : Unbounded_String;  -- Nom du fichier dans lequel écrire les résultats
   Tab_routage : T_Liste; -- Table de routage
   Entree : File_Type;  -- Fichier d'entrée
	Sortie : File_Type;  -- Fichier de sortie
   Cache : T_Liste;  -- Table de cache
   Nb_demande : Integer;
   Nb_defaut : Integer;
   taux : Float;
	
begin
   -- Comprendre la ligne de commande
	Gerer_commandes (Cache_Taille, Politique, Statistique, Table, Paquet, Resultat);


   -- Créer la table de routage
   Initialiser (Tab_routage);
   Table_routage(To_String(Table), Tab_routage);
   Initialiser(Cache);


   -- Mettre en place l'entrée et la sortie
	Create (Sortie, Out_File, To_String (Resultat));
   Ouvrir (To_String(Paquet), Entree);


   -- Traiter les paquets à router
   Traiter_les_paquets (Entree, Sortie, Tab_routage, Cache, Politique, Cache_Taille, Nb_demande, Nb_defaut);


   -- Fermer les fichiers
   Close (Entree);
	Close (Sortie);
   Detruire(Tab_routage);

   -- Agir sur la ligne de commande
   -- Agir selon Statistique
   if Statistique then
      New_Line;
      Put("Nombre de demande de routage : ");
      Put(Nb_demande, 2);
      New_Line;
      Put("Nombre de défeut de Cache : ");
      Put(Nb_defaut, 2);
      New_Line;
      Put("Taux de défaut de cache : ");
      taux := Float(Nb_defaut) / Float(Nb_demande);
      Put(taux);
   else
      Null;
   end if;

end Routage;