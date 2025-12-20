-- fichier : tester_fonctions_globales.adb
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Fonctions_globales;    use Fonctions_globales;
use Fonctions_globales.LCA_routeur_simple;
with Sda_Exceptions;        use Sda_Exceptions;
with Routeur_exceptions;    use Routeur_exceptions;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Tester_Fonctions_Globales is

   -- Valeurs de test pour les adresses IP
   IP1 : constant T_Adresse_IP := Id_ad_IP("192.168.1.1");
   IP2 : constant T_Adresse_IP := Id_ad_IP("10.0.0.1");
   IP3 : constant T_Adresse_IP := Id_ad_IP("172.16.0.1");
   IP4 : constant T_Adresse_IP := Id_ad_IP("147.127.18.80");
   IP5 : constant T_Adresse_IP := Id_ad_IP("212.212.212.212");

   -- Valeurs de test pour les interfaces
   Eth0 : constant Unbounded_String := To_Unbounded_String("eth0");
   Eth1 : constant Unbounded_String := To_Unbounded_String("eth1");
   Eth2 : constant Unbounded_String := To_Unbounded_String("eth2");

   -- Cases de test
   Case1 : constant T_Case := 
      (Destination => Id_ad_IP("147.127.16.0"), 
       Masque => Id_ad_IP("255.255.240.0"), 
       Int => Eth0);
       
   Case2 : constant T_Case := 
      (Destination => Id_ad_IP("147.127.18.0"), 
       Masque => Id_ad_IP("255.255.255.0"), 
       Int => Eth1);
       
   Case3 : constant T_Case := 
      (Destination => Id_ad_IP("147.127.0.0"), 
       Masque => Id_ad_IP("255.255.255.0"), 
       Int => Eth2);
       
   Case4 : constant T_Case := 
      (Destination => Id_ad_IP("212.0.0.0"), 
       Masque => Id_ad_IP("255.0.0.0"), 
       Int => Eth0);
       
   Case5 : constant T_Case := 
      (Destination => Id_ad_IP("0.0.0.0"), 
       Masque => Id_ad_IP("0.0.0.0"), 
       Int => Eth0);

   -- Package de test générique
   generic
      K1, K2, K3, K4, K5 : Integer;
      V1, V2, V3, V4, V5 : T_Case;
   package Testeur is
      
      procedure Tester_Tout;
      
      -- Tests individuels
      procedure Tester_Id_ad_IP;
      procedure Tester_Association_ad_des;
      procedure Tester_Ouvrir;
      procedure Tester_Table_routage;
      procedure Tester_Identifier_commande;
      procedure Tester_Traiter_les_paquets;
      
   end Testeur;

   package body Testeur is
      
      procedure Tester_Id_ad_IP is
         Adresse : T_Adresse_IP;
      begin
         Put("Tester_Id_ad_IP : ");
         
         -- Test d'adresses valides
         Adresse := Id_ad_IP("192.168.1.1");
         pragma Assert(Adresse = 16#C0A80101#);
         
         Adresse := Id_ad_IP("10.0.0.1");
         pragma Assert(Adresse = 16#0A000001#);
         
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
         
         begin
            Adresse := Id_ad_IP("192.168.1.1.1");
            pragma Assert(False);
         exception
            when Adresse_IP_Introuvable_Error =>
               null;
         end;
         
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR dans Tester_Id_ad_IP");
            raise;
      end Tester_Id_ad_IP;
      
      procedure Tester_Association_ad_des is
         Table : T_LCA;
         Resultat : Unbounded_String;
      begin
         Put("Tester_Association_ad_des : ");
         
         -- Créer une table de routage de test
         Initialiser(Table);
         Enregistrer(Table, 1, V1); -- 147.127.16.0/20 -> eth0
         Enregistrer(Table, 2, V2); -- 147.127.18.0/24 -> eth1
         Enregistrer(Table, 3, V3); -- 147.127.0.0/24 -> eth2
         Enregistrer(Table, 4, V4); -- 212.0.0.0/8 -> eth0
         Enregistrer(Table, 5, V5); -- 0.0.0.0/0 -> eth0 (route par défaut)
         
         -- Test 1: Adresse qui correspond à une route spécifique
         -- 147.127.18.80 devrait correspondre à V2 (masque plus long)
         Resultat := Association_ad_des(Table, Id_ad_IP("147.127.18.80"));
         pragma Assert(Resultat = Eth1);
         
         -- Test 2: Adresse qui correspond à une route moins spécifique
         -- 147.127.19.1 devrait correspondre à V1 (147.127.16.0/20)
         Resultat := Association_ad_des(Table, Id_ad_IP("147.127.19.1"));
         pragma Assert(Resultat = Eth0);
         
         -- Test 3: Adresse qui correspond à la route par défaut
         -- 8.8.8.8 devrait correspondre à V5
         Resultat := Association_ad_des(Table, Id_ad_IP("8.8.8.8"));
         pragma Assert(Resultat = Eth0);
         
         -- Test 4: Adresse qui correspond à une route de longueur moyenne
         -- 212.212.212.212 devrait correspondre à V4 (212.0.0.0/8)
         Resultat := Association_ad_des(Table, Id_ad_IP("212.212.212.212"));
         pragma Assert(Resultat = Eth0);
         
         -- Test 5: Adresse sans correspondance (doit lever une exception)
         -- à la route par défaut, donc ce test est modifié
         begin
            Resultat := Association_ad_des(Table, Id_ad_IP("192.168.1.1"));
            pragma Assert(Resultat = Eth0);
         exception
            when Adresse_IP_Introuvable_Error =>
               null; -- Si pas de route par défaut
         end;
         
         Detruire(Table);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR dans Tester_Association_ad_des");
            raise;
      end Tester_Association_ad_des;
      
      procedure Tester_Ouvrir is
         Fichier : File_Type;
         Nom_Fichier : constant String := "test_fichier.txt";
      begin
         Put("Tester_Ouvrir : ");
         
         -- Créer un fichier de test
         Create(Fichier, Out_File, Nom_Fichier);
         Put_Line(Fichier, "Ligne de test");
         Close(Fichier);
         
         -- Tester l'ouverture d'un fichier existant
         Ouvrir(Nom_Fichier, Fichier);
         pragma Assert(not End_Of_File(Fichier));
         Close(Fichier);
         
         -- Tester l'ouverture d'un fichier inexistant 
         begin
            Ouvrir("fichier_inexistant.txt", Fichier);
            pragma Assert(False); 
         exception
            when Fichier_Inconnu_Error =>
               null; 
         end;
         
         Delete(Fichier);
         
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR dans Tester_Ouvrir");
            raise;
      end Tester_Ouvrir;
      
      procedure Tester_Table_routage is
         Table : T_LCA;
         Nom_Fichier : constant String := "test_table_routage.txt";
         Fichier : File_Type;
      begin
         Put("Tester_Table_routage : ");
         
         -- Créer un fichier de table de routage de test
         Create(Fichier, Out_File, Nom_Fichier);
         Put_Line(Fichier, "147.127.16.0 255.255.240.0 eth0");
         Put_Line(Fichier, "147.127.18.0 255.255.255.0 eth1");
         Put_Line(Fichier, "0.0.0.0 0.0.0.0 eth0");
         Close(Fichier);
         
         -- Tester la création de la table
         Initialiser(Table);
         Table_routage(Nom_Fichier, Table);
         
         -- Vérifier que la table a la bonne taille
         pragma Assert(Taille(Table) = 3);
         
         -- Vérifierqu'on peut accéder aux éléments
         declare
            Valeur : T_Case;
         begin
            Valeur := La_Valeur(Table, 1);
            pragma Assert(Valeur.Int = Eth0);
            
            Valeur := La_Valeur(Table, 2);
            pragma Assert(Valeur.Int = Eth1);
            
            Valeur := La_Valeur(Table, 3);
            pragma Assert(Valeur.Int = Eth0);
         end;
         
         Detruire(Table);
         Delete(Fichier);
         
         -- Tester avec un fichier inexistant
         begin
            Initialiser(Table);
            Table_routage("fichier_inexistant.txt", Table);
            pragma Assert(False);
         exception
            when Fichier_Inconnu_Error =>
               null; 
         end;
         
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR dans Tester_Table_routage");
            raise;
      end Tester_Table_routage;
      
      procedure Tester_Identifier_commande is
         Table : T_LCA;
      begin
         Put("Tester_Identifier_commande : ");
         
         Initialiser(Table);
         Enregistrer(Table, 1, V1);
         
         -- Tester les commandes valides
         begin
            Identifier_commande("table", 1, Table);
            null; 
         exception
            when others =>
               pragma Assert(False);
         end;
         
         begin
            Identifier_commande("fin", 2, Table);
            pragma Assert(False); 
         exception
            when End_Error =>
               null; 
         end;
         
         -- Tester une commande invalide
         begin
            Identifier_commande("commande_invalide", 3, Table);
            pragma Assert(False);
         exception
            when Commande_Inconnu_Error =>
               null; 
         end;
         
         Detruire(Table);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR dans Tester_Identifier_commande");
            raise;
      end Tester_Identifier_commande;
      
      procedure Tester_Traiter_les_paquets is
         Table : T_LCA;
         Entree : File_Type;
         Sortie : File_Type;
         Nom_Entree : constant String := "test_paquets.txt";
         Nom_Sortie : constant String := "test_resultats.txt";
      begin
         Put("Tester_Traiter_les_paquets : ");
         
         -- Créer un fichier d'entrée avec des paquets et des commandes
         Create(Entree, Out_File, Nom_Entree);
         Put_Line(Entree, "table");
         Put_Line(Entree, "147.127.18.80");
         Put_Line(Entree, "212.212.212.212");
         Put_Line(Entree, "fin");
         Put_Line(Entree, "147.127.19.1"); 
         Close(Entree);
         
         -- Créer une table de routage de test
         Initialiser(Table);
         Enregistrer(Table, 1, V1); -- 147.127.16.0/20 -> eth0
         Enregistrer(Table, 2, V2); -- 147.127.18.0/24 -> eth1
         Enregistrer(Table, 3, V4); -- 212.0.0.0/8 -> eth0
         
         -- Ouvrir les fichiers
         Ouvrir(Nom_Entree, Entree);
         Create(Sortie, Out_File, Nom_Sortie);
         
         -- Traiter les paquets
         Traiter_les_paquets(Entree, Sortie, Table);
         
         Close(Entree);
         Close(Sortie);
         
         -- Vérifier que le fichier de sortie a été créé et contient des résultats
         Open(Entree, In_File, Nom_Sortie);
         pragma Assert(not End_Of_File(Entree));
         Close(Entree);
         
         -- Nettoyer
         Detruire(Table);
         Delete(Entree);
         Delete(Sortie);
         
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR dans Tester_Traiter_les_paquets");
            raise;
      end Tester_Traiter_les_paquets;
      
      procedure Tester_Tout is
      begin
         Tester_Id_ad_IP;
         Tester_Association_ad_des;
         Tester_Ouvrir;
         Tester_Table_routage;
         Tester_Identifier_commande;
         Tester_Traiter_les_paquets;
      end Tester_Tout;
      
   end Testeur;
   
   -- Instanciation du testeur
   package Testeur_Instance is new Testeur(
      K1 => 1,
      K2 => 2,
      K3 => 3,
      K4 => 4,
      K5 => 5,
      V1 => Case1,
      V2 => Case2,
      V3 => Case3,
      V4 => Case4,
      V5 => Case5
   );
   
begin
   Put_Line("Début des tests des fonctions globales...");
   New_Line;
   
   Testeur_Instance.Tester_Tout;
   
   New_Line;
   Put_Line("Tous les tests ont réussi !");
   
exception
   when others =>
      Put_Line("Certains tests ont échoué !");
      raise;
end Tester_Fonctions_Globales;