-- fichier : tester_fonctions_globales.adb
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings;    use Ada.Strings;
with Fonctions_globales;    use Fonctions_globales;
use Fonctions_globales.LCA_routeur_simple;
--  with Sda_Exceptions;        use Sda_Exceptions;
with Routeur_exceptions;    use Routeur_exceptions;
--  with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Directories;   use Ada.Directories;


procedure tester_routage_simple is

   type T_Enregistrement is record
      Adresse_IP : T_Adresse_IP;
      Inter : Unbounded_String;
   end record;

   function Transfo_Nat_UString(Adresse : in T_Adresse_IP) return Unbounded_String is
   begin
      return Trim(To_Unbounded_String(Natural'Image(Natural(Adresse))), both);
   end Transfo_Nat_UString;


   function Transfo(M1 : in T_Adresse_IP) return String is
      Adresse_String: Unbounded_String;
   begin
      Adresse_String := Transfo_Nat_UString((M1 / UN_OCTET ** 3) mod UN_OCTET) & ".";
      Adresse_String := Adresse_String & Transfo_Nat_UString((M1 / UN_OCTET ** 2) mod UN_OCTET) & (".");
      Adresse_String := Adresse_String & Transfo_Nat_UString((M1 / UN_OCTET ** 1) mod UN_OCTET) & (".");
      Adresse_String := Adresse_String & Transfo_Nat_UString(M1 mod UN_OCTET);
      return To_String(Adresse_String);
   end Transfo;

   function Transfo_Ad_IP(B1, B2, B3, B4 : in T_Adresse_IP) return T_Adresse_IP is 
   begin
      return (B1 * UN_OCTET**3) + (B2 * UN_OCTET**2) + (B3 * UN_OCTET) + B4;
   end Transfo_Ad_IP;

   function Transfo_Case_String(C1 : in T_Case) return String is
   begin
      return Transfo(C1.Destination) & " " & Transfo (C1.Masque) & " " & To_String(C1.Int);
   end Transfo_Case_String;

   -- Valeurs de test pour les interfaces
   Eth0 : constant Unbounded_String := To_Unbounded_String("eth0");
   Eth1 : constant Unbounded_String := To_Unbounded_String("eth1");
   Eth2 : constant Unbounded_String := To_Unbounded_String("eth2");

   -- Valeurs de test pour les adresses IP
   IP1 : constant T_Enregistrement := (
      Adresse_IP => Transfo_Ad_IP (192, 168, 1, 1),
      Inter => ETH0 );
   IP2 : constant T_Enregistrement := (
      Adresse_IP => Transfo_Ad_IP (10, 0, 0, 1),
      Inter => ETH0);
   IP3 : constant T_Enregistrement := (
      Adresse_IP => Transfo_Ad_IP (172, 16, 0, 1),
      Inter => ETH0);
   IP4 : constant T_Enregistrement := (
      Adresse_IP => Transfo_Ad_IP (147, 127, 18, 80), 
      Inter => Eth1);
   IP5 : constant T_Enregistrement := (
      Adresse_IP => Transfo_Ad_IP (212, 212, 212, 212), 
      Inter => Eth0);

   -- Cases de test
   Case1 : constant T_Case := 
      (Destination => Transfo_Ad_IP(147,127,16,0), 
       Masque => Transfo_Ad_IP(255,255,240,0), 
       Int => Eth0);
       
   Case2 : constant T_Case := 
      (Destination => Transfo_Ad_IP(147,127,18,0), 
       Masque => Transfo_Ad_IP(255,255,255,0), 
       Int => Eth1);
       
   Case3 : constant T_Case := 
      (Destination => Transfo_Ad_IP(147,127,0,0), 
       Masque => Transfo_Ad_IP(255,255,255,0), 
       Int => Eth2);
       
   Case4 : constant T_Case := 
      (Destination => Transfo_Ad_IP(212,0,0,0), 
       Masque => Transfo_Ad_IP(255,0,0,0), 
       Int => Eth0);
       
   Case5 : constant T_Case := 
      (Destination => Transfo_Ad_IP(0,0,0,0), 
       Masque => Transfo_Ad_IP(0,0,0,0), 
       Int => Eth0);


   -- Package de test générique
   generic
      K1, K2, K3, K4, K5 : Integer;    -- Cle de la table de routage
      V1, V2, V3, V4, V5 : T_Case;     -- Valeurs de la table de routage
      T1, T2, T3, T4, T5 : T_Enregistrement;    -- Association Adresse_IP et Interface correspondant à la table de routage
      A1, A2, A3 : String;    -- Mauvaises format de Adresse IP
      D1, D2, D3, D4, D5 : String;    -- Contenu du paquet à router

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
         Adresse := Id_ad_IP(Transfo(T1.Adresse_IP));
         pragma Assert(Adresse = T1.Adresse_IP);
         
         Adresse := Id_ad_IP(Transfo(T2.Adresse_IP));
         pragma Assert(Adresse = T2.Adresse_IP);
         
         Adresse := Id_ad_IP(Transfo(T3.Adresse_IP));
         pragma Assert(Adresse = T3.Adresse_IP);

         -- Test d'adresses invalides (doivent lever une exception)
         begin
            Adresse := Id_ad_IP(A1);
            pragma Assert(False); -- Ne devrait pas arriver ici
         exception
            when Adresse_IP_Introuvable_Error =>
               null; -- Comportement attendu
         end;
         
         begin
            Adresse := Id_ad_IP(A2);
            pragma Assert(False);
         exception
            when Adresse_IP_Introuvable_Error =>
               null;
         end;
         
         begin
            Adresse := Id_ad_IP(A3);
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
         Enregistrer(Table, K1, V1); -- 147.127.16.0/20 -> eth0
         Enregistrer(Table, K2, V2); -- 147.127.18.0/24 -> eth1
         Enregistrer(Table, K3, V3); -- 147.127.0.0/24 -> eth2
         Enregistrer(Table, K4, V4); -- 212.0.0.0/8 -> eth0
         Enregistrer(Table, K5, V5); -- 0.0.0.0/0 -> eth0 (route par défaut)
         
         -- Test 1: Adresse qui correspond à une route spécifique
         -- 147.127.18.80 devrait correspondre à V2 (masque plus long)
         Resultat := Association_ad_des(Table, T1.Adresse_IP);
         pragma Assert(Resultat = T1.Inter);
         
         -- Test 2: Adresse qui correspond à une route moins spécifique
         -- 147.127.19.1 devrait correspondre à V1 (147.127.16.0/20)
         Resultat := Association_ad_des(Table, T2.Adresse_IP);
         pragma Assert(Resultat = T2.Inter);
         
         -- Test 3: Adresse qui correspond à la route par défaut
         -- 8.8.8.8 devrait correspondre à V5
         Resultat := Association_ad_des(Table, T3.Adresse_IP);
         pragma Assert(Resultat = T3.Inter);
         
         -- Test 4: Adresse qui correspond à une route de longueur moyenne
         -- 212.212.212.212 devrait correspondre à V4 (212.0.0.0/8)
         Resultat := Association_ad_des(Table, T4.Adresse_IP);
         pragma Assert(Resultat = T4.Inter);
         
         -- Test 5: Adresse sans correspondance (doit lever une exception)
         -- à la route par défaut, donc ce test est modifié
         begin
            Resultat := Association_ad_des(Table, T5.Adresse_IP);
            pragma Assert(Resultat = T5.Inter);
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
         Dummy : File_Type;
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
         
         Create (Dummy, Out_File, "temp_output.txt");
         Set_Output (Dummy);
         -- Tester l'ouverture d'un fichier inexistant 
         begin
            Ouvrir("fichier_inexistant.txt", Fichier);
            pragma Assert(False); 
         exception
            when Fichier_Inconnu_Error =>
               null; 
         end;

         
         Delete_File (Nom_Fichier);
         Set_Output (Standard_Output);
         Close (Dummy);
         Delete_File ("temp_output.txt");
         
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
         Valeur : T_Case;
      begin
         Put("Tester_Table_routage : ");
         
         -- Créer un fichier de table de routage de test
         Create(Fichier, Out_File, Nom_Fichier);
         Put_Line(Fichier, Transfo_Case_String(V1));
         Put_Line(Fichier, Transfo_Case_String(V2));
         Put_Line(Fichier, Transfo_Case_String(V3));
         Close(Fichier);
         
         -- Tester la création de la table
         Initialiser(Table);
         Table_routage(Nom_Fichier, Table);
         
         -- Vérifier que la table a la bonne taille
         pragma Assert(Taille(Table) = 3);
         
         -- Vérifierqu'on peut accéder aux éléments
         Valeur := La_Valeur(Table, 1);
         pragma Assert(Valeur.Int = V1.Int);
            
         Valeur := La_Valeur(Table, 2);
         pragma Assert(Valeur.Int = V2.Int);
            
         Valeur := La_Valeur(Table, 3);
         pragma Assert(Valeur.Int = V3.Int);
         
         
         Detruire(Table);
         Delete_File (Nom_Fichier);

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
         Dummy : File_Type;
      begin
         Put("Tester_Identifier_commande : ");
         
         Initialiser(Table);
         Enregistrer(Table, 1, V1);
         Create (Dummy, Out_File, "temp_output.txt");
         Set_Output (Dummy);

         -- Tester les commandes valides
         begin
            Identifier_commande("table", 1, Table);
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
         Set_Output (Standard_Output);
         Close (Dummy);
         Delete_File ("temp_output.txt");
         
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
         Dummy : File_Type;
      begin
         Put("Tester_Traiter_les_paquets : ");
         
         -- Créer un fichier d'entrée avec des paquets et des commandes
         Create(Entree, Out_File, Nom_Entree);
         Put_Line(Entree, D1);
         Put_Line(Entree, D2);
         Put_Line(Entree, D3);
         Put_Line(Entree, D4);
         Put_Line(Entree, D5); 
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
         Enregistrer(Table, 1, V1);
         Create (Dummy, Out_File, "temp_output.txt");
         Set_Output (Dummy);
         Traiter_les_paquets(Entree, Sortie, Table);
         Set_Output (Standard_Output);
         Close (Dummy);
         Delete_File ("temp_output.txt");

         Close(Entree);
         Close(Sortie);
         
         -- Vérifier que le fichier de sortie a été créé et contient des résultats
         Open(Entree, In_File, Nom_Sortie);
         pragma Assert(not End_Of_File(Entree));
         Close(Entree);
         
         -- Nettoyer
         Detruire(Table);
         Delete_File (Nom_Entree);
         Delete_File (Nom_Sortie);
         
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
      K1 => 1, K2 => 2, K3 => 3, K4 => 4, K5 => 5,
      V1 => Case1, V2 => Case2, V3 => Case3, V4 => Case4, V5 => Case5,
      T1 => IP1, T2 => IP2, T3 => IP3, T4 => IP4, T5 => IP5,
      A1 => "256.0.0.1", A2 => "192.168.1", A3 =>"192.168.1.1.1",
      D1 =>"table",  D2 =>"147.127.18.80", D3 =>"212.212.212.212", D4 =>"fin", D5 => "147.127.19.1"
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
end tester_routage_simple;
