with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;       use Ada.Directories;

with Fonctions_globales;    use Fonctions_globales;
with Cache_Arbre;           use Cache_Arbre;
with SDA_Exceptions;        use SDA_Exceptions;
with Routeur_exceptions;    use Routeur_exceptions;

use Fonctions_globales.LCA_routeur_simple;

procedure tester_routeur_la is

   -- Adresses IP de test
   IP1 : constant T_Adresse_IP := Id_ad_IP("192.168.1.1");
   IP2 : constant T_Adresse_IP := Id_ad_IP("10.0.0.1");
   IP3 : constant T_Adresse_IP := Id_ad_IP("172.16.0.1");
   
   -- Interfaces de test
   Eth0 : constant Unbounded_String := To_Unbounded_String("eth0");
   Eth1 : constant Unbounded_String := To_Unbounded_String("eth1");
   Eth2 : constant Unbounded_String := To_Unbounded_String("eth2");
   Eth3 : constant Unbounded_String := To_Unbounded_String("eth3");

   -- Fichiers de test
   Fichier_Table_Test : constant String := "table_test.txt";
   Fichier_Paquets_Test : constant String := "paquets_test.txt";
   Fichier_Resultats_Test : constant String := "resultats_test.txt";

   -- Créer une table de routage de test
   procedure Creer_Table_Test is
      Fichier : File_Type;
   begin
      Create(Fichier, Out_File, Fichier_Table_Test);
      Put_Line(Fichier, "192.168.1.0 255.255.255.0 eth0");
      Put_Line(Fichier, "10.0.0.0 255.255.0.0 eth1");
      Put_Line(Fichier, "172.16.0.0 255.255.0.0 eth2");
      Put_Line(Fichier, "0.0.0.0 0.0.0.0 eth3"); -- Route par défaut
      Close(Fichier);
   exception
      when others =>
         Put_Line("Erreur lors de la création de la table de test");
   end Creer_Table_Test;

   -- Créer un fichier de paquets de test
   procedure Creer_Paquets_Test is
      Fichier : File_Type;
   begin
      Create(Fichier, Out_File, Fichier_Paquets_Test);
      Put_Line(Fichier, "192.168.1.1");
      Put_Line(Fichier, "10.0.0.1");
      Put_Line(Fichier, "172.16.0.1");
      Put_Line(Fichier, "8.8.8.8"); -- Doit correspondre à la route par défaut
      Put_Line(Fichier, "table");
      Put_Line(Fichier, "cache");
      Put_Line(Fichier, "stat");
      Put_Line(Fichier, "fin");
      Put_Line(Fichier, "192.168.2.1"); -- Cette ligne ne devrait pas être traitée
      Close(Fichier);
   exception
      when others =>
         Put_Line("Erreur lors de la création des paquets de test");
   end Creer_Paquets_Test;

   -- Nettoyer les fichiers de test
   procedure Nettoyer_Fichiers_Test is
   begin
      if Exists(Fichier_Table_Test) then
         Delete_File(Fichier_Table_Test);
      end if;
      if Exists(Fichier_Paquets_Test) then
         Delete_File(Fichier_Paquets_Test);
      end if;
      if Exists(Fichier_Resultats_Test) then
         Delete_File(Fichier_Resultats_Test);
      end if;
   exception
      when others =>
         null;
   end Nettoyer_Fichiers_Test;

   -- Test de l'initialisation du routeur
   procedure Tester_Initialisation is
      Tab_Routage : T_LCA;
      Cache : T_Cache;
   begin
      Put("Tester_Initialisation : ");
      Initialiser(Tab_Routage);
      Cache_Arbre.Initialiser(Cache, 10);
      pragma Assert(Est_Vide(Tab_Routage));
      pragma Assert(Taille(Cache) = 0);
      Detruire(Tab_Routage);
      Detruire(Cache);
      Put_Line("OK");
   exception
      when others =>
         Put_Line("ERREUR");
         raise;
   end Tester_Initialisation;

   -- Test du chargement de la table de routage
   procedure Tester_Chargement_Table is
      Tab_Routage : T_LCA;
   begin
      Put("Tester_Chargement_Table : ");
      Creer_Table_Test;
      Initialiser(Tab_Routage);
      Table_routage(Fichier_Table_Test, Tab_Routage);
      pragma Assert(Taille(Tab_Routage) = 4);
      Detruire(Tab_Routage);
      Put_Line("OK");
   exception
      when others =>
         Put_Line("ERREUR");
         raise;
   end Tester_Chargement_Table;

   -- Test de la recherche dans la table de routage
   procedure Tester_Recherche_Table is
      Tab_Routage : T_LCA;
      Valeur : T_Case;
   begin
      Put("Tester_Recherche_Table : ");
      Creer_Table_Test;
      Initialiser(Tab_Routage);
      Table_routage(Fichier_Table_Test, Tab_Routage);
      
      -- Récupérer la route pour 192.168.1.1
      Valeur := La_Valeur(Tab_Routage, 1);
      pragma Assert(Valeur.Int = Eth0);
      
      -- Récupérer la route par défaut
      Valeur := La_Valeur(Tab_Routage, 4);
      pragma Assert(Valeur.Int = Eth3);
      
      -- Vérifier que l'association fonctionne
      declare
         Int : Unbounded_String;
      begin
         Int := Association_ad_des(Tab_Routage, IP1);
         pragma Assert(Int = Eth0);
         
         Int := Association_ad_des(Tab_Routage, Id_ad_IP("8.8.8.8"));
         pragma Assert(Int = Eth3);
      end;
      
      Detruire(Tab_Routage);
      Put_Line("OK");
   exception
      when others =>
         Put_Line("ERREUR");
         raise;
   end Tester_Recherche_Table;

   -- Test de la gestion des commandes (simplifié - ne modifie pas les arguments)
   procedure Tester_Gestion_Commandes is
      Cache_Size : Integer;
      Politique : Tab_Politique;
      Statistique : Boolean;
      Table : Unbounded_String;
      Paquet : Unbounded_String;
      Resultat : Unbounded_String;
   begin
      Put("Tester_Gestion_Commandes : ");
      
      -- Tester que Gerer_commandes ne lève pas d'exception
      -- (nous ne pouvons pas modifier les arguments en cours d'exécution)
      Gerer_commandes(Cache_Size, Politique, Statistique, Table, Paquet, Resultat);
      
      -- Vérifier que les valeurs par défaut sont raisonnables
      pragma Assert(Cache_Size >= 0);
      -- Politique doit être FIFO, LRU ou LFU (vérifié par le type)
      -- Statistique doit être True ou False
      
      Put_Line("OK (vérification basique)");
   exception
      when others =>
         Put_Line("ERREUR");
         raise;
   end Tester_Gestion_Commandes;

   -- Test de la procédure Ecrire
   procedure Tester_Ecrire is
      Fichier : File_Type;
      Nom_Fichier : constant String := "test_ecrire.txt";
   begin
      Put("Tester_Ecrire : ");
      Create(Fichier, Out_File, Nom_Fichier);
      Ecrire(Fichier, IP1, To_String(Eth0));
      Close(Fichier);
      
      -- Vérifier que le fichier a été créé et contient la ligne attendue
      Open(Fichier, In_File, Nom_Fichier);
      declare
         Ligne : constant String := Get_Line(Fichier);
      begin
         -- La ligne devrait contenir l'adresse IP et l'interface
         pragma Assert(Ligne'Length > 0);
         -- Vérifier que ça commence par l'adresse IP
         pragma Assert(Ligne(Ligne'First) in '0'..'9');
      end;
      Close(Fichier);
      Delete_File(Nom_Fichier);
      Put_Line("OK");
   exception
      when others =>
         if Is_Open(Fichier) then
            Close(Fichier);
         end if;
         if Exists(Nom_Fichier) then
            Delete_File(Nom_Fichier);
         end if;
         Put_Line("ERREUR");
         raise;
   end Tester_Ecrire;

   -- Test de la conversion d'adresse IP
   procedure Tester_Id_ad_IP is
      Adresse : T_Adresse_IP;
   begin
      Put("Tester_Id_ad_IP : ");
      
      -- Test d'adresses valides
      Adresse := Id_ad_IP("192.168.1.1");
      pragma Assert(Adresse = IP1);
      
      Adresse := Id_ad_IP("255.255.255.255");
      pragma Assert(Adresse = 16#FFFFFFFF#);
      
      -- Test d'adresses invalides (doivent lever une exception)
      begin
         Adresse := Id_ad_IP("256.0.0.1");
         pragma Assert(False); -- Ne devrait pas arriver ici
      exception
         when Adresse_IP_Introuvable_Error =>
            null; -- Comportement attendu
      end;
      
      begin
         Adresse := Id_ad_IP("192.168.1");
         pragma Assert(False);
      exception
         when Adresse_IP_Introuvable_Error =>
            null;
      end;
      
      Put_Line("OK");
   exception
      when others =>
         Put_Line("ERREUR");
         raise;
   end Tester_Id_ad_IP;

   -- Test du cache avec différentes politiques
   procedure Tester_Cache_Politiques is
      Cache : T_Cache;
   begin
      Put("Tester_Cache_Politiques : ");
      
      -- Test FIFO
      Cache_Arbre.Initialiser(Cache, 2);
      Enregistrer(Cache, IP1, Id_ad_IP("255.255.255.0"), Eth0);
      Enregistrer(Cache, IP2, Id_ad_IP("255.255.0.0"), Eth1);
      Enregistrer(Cache, IP3, Id_ad_IP("255.255.0.0"), Eth2); -- Doit évincer IP1 (FIFO)
      
      begin
         declare
            Resultat : Unbounded_String := Rechercher(Cache, IP1);
         begin
            pragma Assert(False);
         end;
      exception
         when Cle_Absente_Error =>
            null; -- Comportement attendu pour FIFO
      end;
      
      pragma Assert(Rechercher(Cache, IP2) = Eth1);
      pragma Assert(Rechercher(Cache, IP3) = Eth2);
      
      Detruire(Cache);
      
      -- Test LRU
      Cache_Arbre.Initialiser(Cache, 2);
      Enregistrer(Cache, IP1, Id_ad_IP("255.255.255.0"), Eth0);
      Enregistrer(Cache, IP2, Id_ad_IP("255.255.0.0"), Eth1);
      
      -- Accéder à IP1 pour la rendre récemment utilisée
      pragma Assert(Rechercher(Cache, IP1) = Eth0);
      Mettre_A_Jour_LRU(Cache, IP1, Id_ad_IP("255.255.255.0"));
      
      Enregistrer(Cache, IP3, Id_ad_IP("255.255.0.0"), Eth2); -- Doit évincer IP2 (LRU)
      
      begin
         declare
            Resultat : Unbounded_String := Rechercher(Cache, IP2);
         begin
            pragma Assert(False);
         end;
      exception
         when Cle_Absente_Error =>
            null; -- Comportement attendu pour LRU
      end;
      
      Detruire(Cache);
      
      Put_Line("OK");
   exception
      when others =>
         Put_Line("ERREUR");
         raise;
   end Tester_Cache_Politiques;

   -- Test du traitement complet avec fichiers
   procedure Tester_Traitement_Complet is
      Tab_Routage : T_LCA;
      Entree : File_Type;
      Sortie : File_Type;
   begin
      Put("Tester_Traitement_Complet : ");
      
      -- Créer les fichiers de test
      Creer_Table_Test;
      Creer_Paquets_Test;
      
      -- Initialiser la table de routage
      Initialiser(Tab_Routage);
      Table_routage(Fichier_Table_Test, Tab_Routage);
      
      -- Ouvrir les fichiers
      Ouvrir(Fichier_Paquets_Test, Entree);
      Create(Sortie, Out_File, Fichier_Resultats_Test);
      
      -- Traiter les paquets
      Traiter_les_paquets(Entree, Sortie, Tab_Routage);
      
      -- Fermer les fichiers
      Close(Entree);
      Close(Sortie);
      
      -- Vérifier que le fichier de résultats a été créé
      pragma Assert(Exists(Fichier_Resultats_Test));
      
      -- Vérifier le contenu (au moins 4 lignes pour les 4 premières adresses IP)
      Open(Entree, In_File, Fichier_Resultats_Test);
      declare
         Ligne_Count : Integer := 0;
      begin
         while not End_Of_File(Entree) loop
            declare
               Ligne : constant String := Get_Line(Entree);
            begin
               Ligne_Count := Ligne_Count + 1;
               -- Vérifier que chaque ligne contient une adresse IP et une interface
               pragma Assert(Ligne'Length > 0);
            end;
         end loop;
         -- On devrait avoir 4 lignes de résultats
         pragma Assert(Ligne_Count = 4);
      end;
      Close(Entree);
      
      Detruire(Tab_Routage);
      Put_Line("OK");
   exception
      when others =>
         if Is_Open(Entree) then
            Close(Entree);
         end if;
         if Is_Open(Sortie) then
            Close(Sortie);
         end if;
         Put_Line("ERREUR");
         raise;
   end Tester_Traitement_Complet;

begin
   Put_Line("=== Début des tests du routeur avec cache ===");
   New_Line;
   
   -- Exécuter tous les tests
   Tester_Initialisation;
   Tester_Chargement_Table;
   Tester_Recherche_Table;
   Tester_Gestion_Commandes;
   Tester_Ecrire;
   Tester_Id_ad_IP;
   Tester_Cache_Politiques;
   Tester_Traitement_Complet;
   
   -- Nettoyer les fichiers de test à la fin
   Nettoyer_Fichiers_Test;
   
   New_Line;
   Put_Line("=== Tous les tests ont réussi ! ===");
   
exception
   when others =>
      Nettoyer_Fichiers_Test;
      Put_Line("=== Certains tests ont échoué ! ===");
      raise;
end tester_routeur_la;