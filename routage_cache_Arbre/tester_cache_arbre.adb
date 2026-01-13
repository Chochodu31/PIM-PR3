with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with SDA_Exceptions;        use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Fonctions_globales;    use Fonctions_globales;
with Cache_Arbre;           use Cache_Arbre;

procedure Tester_Cache_Arbre is

   -- Adresses IP de test
   IP1 : constant T_Adresse_IP := Id_ad_IP("192.168.1.1");
   IP2 : constant T_Adresse_IP := Id_ad_IP("10.0.0.1");
   IP3 : constant T_Adresse_IP := Id_ad_IP("172.16.0.1");
   IP4 : constant T_Adresse_IP := Id_ad_IP("147.127.18.80");
   IP5 : constant T_Adresse_IP := Id_ad_IP("212.212.212.212");
   
   -- Masques de test
   Masque1 : constant T_Adresse_IP := Id_ad_IP("255.255.255.0");
   Masque2 : constant T_Adresse_IP := Id_ad_IP("255.255.0.0");
   Masque3 : constant T_Adresse_IP := Id_ad_IP("255.0.0.0");
   Masque4 : constant T_Adresse_IP := Id_ad_IP("255.255.240.0");
   
   -- Interfaces de test
   Eth0 : constant Unbounded_String := To_Unbounded_String("eth0");
   Eth1 : constant Unbounded_String := To_Unbounded_String("eth1");
   Eth2 : constant Unbounded_String := To_Unbounded_String("eth2");
   Eth3 : constant Unbounded_String := To_Unbounded_String("eth3");
   
   -- Test générique inspiré de tester_lca.adb
   generic
      Taille_Max : Integer;
      Politique : Tab_Politique;
   package Testeur_Cache is
      procedure Tester_Tout;
   end Testeur_Cache;
   
   package body Testeur_Cache is
      
      procedure Tester_Initialiser is
         Cache : T_Cache;
      begin
         Put("Tester_Initialiser : ");
         Initialiser(Cache, Taille_Max);
         pragma Assert(Taille(Cache) = 0);
         Detruire(Cache);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR");
            raise;
      end Tester_Initialiser;
      
      procedure Tester_Enregistrer is
         Cache : T_Cache;
      begin
         Put("Tester_Enregistrer : ");
         Initialiser(Cache, Taille_Max);
         
         -- Ajout de routes
         Enregistrer(Cache, IP1, Masque1, Eth0);
         pragma Assert(Taille(Cache) = 1);
         
         Enregistrer(Cache, IP2, Masque2, Eth1);
         pragma Assert(Taille(Cache) = 2);
         
         Enregistrer(Cache, IP3, Masque3, Eth2);
         pragma Assert(Taille(Cache) = 3);
         
         -- Remplacement d'une route existante
         Enregistrer(Cache, IP1, Masque1, Eth3);
         pragma Assert(Taille(Cache) = 3);
         
         Detruire(Cache);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR");
            raise;
      end Tester_Enregistrer;
      
      procedure Tester_Rechercher is
         Cache : T_Cache;
         Resultat : Unbounded_String;
      begin
         Put("Tester_Rechercher : ");
         Initialiser(Cache, Taille_Max);
         
         -- Ajout de routes
         Enregistrer(Cache, IP1, Masque1, Eth0);
         Enregistrer(Cache, IP2, Masque2, Eth1);
         
         -- Recherche d'une route présente
         Resultat := Rechercher(Cache, IP1);
         pragma Assert(Resultat = Eth0);
         
         Resultat := Rechercher(Cache, IP2);
         pragma Assert(Resultat = Eth1);
         
         -- Recherche d'une route absente (doit lever Cle_Absente_Error)
         begin
            Resultat := Rechercher(Cache, IP3);
            pragma Assert(False);
         exception
            when Cle_Absente_Error =>
               null; -- Comportement attendu
         end;
         
         Detruire(Cache);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR");
            raise;
      end Tester_Rechercher;
      
      procedure Tester_Supprimer is
         Cache : T_Cache;
      begin
         Put("Tester_Supprimer : ");
         Initialiser(Cache, Taille_Max);
         
         -- Ajout de routes
         Enregistrer(Cache, IP1, Masque1, Eth0);
         Enregistrer(Cache, IP2, Masque2, Eth1);
         Enregistrer(Cache, IP3, Masque3, Eth2);
         
         -- Suppression d'une route
         Supprimer(Cache, IP1, Masque1);
         pragma Assert(Taille(Cache) = 2);
         
         -- Vérification que la route supprimée n'est plus accessible
         begin
            declare
               R : Unbounded_String := Rechercher(Cache, IP1);
            begin
               pragma Assert(False);
            end;
         exception
            when Cle_Absente_Error =>
               null; -- Comportement attendu
         end;
         
         -- Les autres routes doivent être intactes
         pragma Assert(Rechercher(Cache, IP2) = Eth1);
         
         Detruire(Cache);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR");
            raise;
      end Tester_Supprimer;
      
      procedure Tester_Eviction is
         Cache : T_Cache;
      begin
         Put("Tester_Eviction_" & Tab_Politique'Image(Politique) & " : ");
         Initialiser(Cache, 2); -- Cache de taille 2 seulement
         
         -- Ajout de 2 routes (cache plein)
         Enregistrer(Cache, IP1, Masque1, Eth0);
         Enregistrer(Cache, IP2, Masque2, Eth1);
         pragma Assert(Taille(Cache) = 2);
         
         -- Ajout d'une 3ème route (doit déclencher l'éviction)
         Enregistrer(Cache, IP3, Masque3, Eth2);
         
         -- Selon la politique, une route a dû être évincée
         -- Note: Le comportement exact dépend de l'implémentation
         pragma Assert(Taille(Cache) = 2);
         
         Detruire(Cache);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR");
            raise;
      end Tester_Eviction;
      
      procedure Tester_Statistiques is
         Cache : T_Cache;
         Nb_Defauts, Nb_Demandes : Integer;
         Taux : Float;
      begin
         Put("Tester_Statistiques : ");
         Initialiser(Cache, Taille_Max);
         
         -- Ajout et recherche pour générer des statistiques
         Enregistrer(Cache, IP1, Masque1, Eth0);
         
         -- Recherche réussie
         declare
            R : Unbounded_String := Rechercher(Cache, IP1);
         begin
            null;
         end;
         
         -- Recherche échouée (défaut)
         begin
            declare
               R : Unbounded_String := Rechercher(Cache, IP2);
            begin
               pragma Assert(False);
            end;
         exception
            when Cle_Absente_Error =>
               null;
         end;
         
         -- Obtention des statistiques
         Obtenir_Statistiques(Cache, Nb_Defauts, Nb_Demandes, Taux);
         
         -- Vérifications
         pragma Assert(Nb_Demandes = 2); -- 2 recherches effectuées
         pragma Assert(Nb_Defauts = 1);  -- 1 défaut
         pragma Assert(Taux = 0.5);      -- Taux de défauts = 1/2
         
         Detruire(Cache);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR");
            raise;
      end Tester_Statistiques;
      
      procedure Tester_Afficher is
         Cache : T_Cache;
      begin
         Put("Tester_Afficher : ");
         Initialiser(Cache, Taille_Max);
         
         -- Ajout de quelques routes
         Enregistrer(Cache, IP1, Masque1, Eth0);
         Enregistrer(Cache, IP2, Masque2, Eth1);
         
         -- L'affichage ne devrait pas planter
         Afficher(Cache);
         
         Detruire(Cache);
         Put_Line("OK");
      exception
         when others =>
            Put_Line("ERREUR");
            raise;
      end Tester_Afficher;
      
      procedure Tester_Tout is
      begin
         Tester_Initialiser;
         Tester_Enregistrer;
         Tester_Rechercher;
         Tester_Supprimer;
         Tester_Eviction;
         Tester_Statistiques;
         Tester_Afficher;
      end Tester_Tout;
      
   end Testeur_Cache;
   
   -- Instanciation des testeurs pour chaque politique
   package Testeur_FIFO is new Testeur_Cache(Taille_Max => 5, Politique => FIFO);
   package Testeur_LRU is new Testeur_Cache(Taille_Max => 5, Politique => LRU);
   package Testeur_LFU is new Testeur_Cache(Taille_Max => 5, Politique => LFU);
   
begin
   Put_Line("=== Début des tests du cache arbre ===");
   New_Line;
   
   Put_Line("--- Tests avec politique FIFO ---");
   Testeur_FIFO.Tester_Tout;
   New_Line;
   
   Put_Line("--- Tests avec politique LRU ---");
   Testeur_LRU.Tester_Tout;
   New_Line;
   
   Put_Line("--- Tests avec politique LFU ---");
   Testeur_LFU.Tester_Tout;
   New_Line;
   
   Put_Line("=== Tous les tests ont réussi ! ===");
   
exception
   when others =>
      Put_Line("=== Certains tests ont échoué ! ===");
      raise;
end Tester_Cache_Arbre;