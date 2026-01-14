with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Fonctions_globales;    use Fonctions_globales;
with Cache_Arbre;           use Cache_Arbre;
with SDA_Exceptions;        use SDA_Exceptions;
with Routeur_exceptions;    use Routeur_exceptions;

procedure Tester_Routeur_LA is

   IP_1 : constant T_Adresse_IP := Id_ad_IP("192.168.1.1");
   IP_2 : constant T_Adresse_IP := Id_ad_IP("192.168.1.10");
   IP_3 : constant T_Adresse_IP := Id_ad_IP("10.0.0.5");
   
   M_24 : constant T_Adresse_IP := Id_ad_IP("255.255.255.0");
   M_32 : constant T_Adresse_IP := Id_ad_IP("255.255.255.255");
   M_8  : constant T_Adresse_IP := Id_ad_IP("255.0.0.0");

   function Trouver_Route_LPM(Tab : in LCA_routeur_simple.T_LCA; IP : in T_Adresse_IP) return T_Case is
      Meilleure_Route : T_Case;
      Trouve          : Boolean := False;

      procedure Examiner(Cle : in Integer; Valeur : in T_Case) is
      begin
         if (IP and Valeur.Masque) = Valeur.Destination then
            if not Trouve or else Valeur.Masque > Meilleure_Route.Masque then
               Meilleure_Route := Valeur;
               Trouve := True;
            end if;
         end if;
      end Examiner;

      procedure Parcourir_LCA is new LCA_routeur_simple.Faire_Pour_Chaque(Examiner);
   begin
      Parcourir_LCA(Tab);
      if not Trouve then
         raise Adresse_IP_Introuvable_Error;
      end if;
      return Meilleure_Route;
   end Trouver_Route_LPM;

   procedure Tester_Integration_Hit_Miss is
      Cache : T_Cache;
      Table : LCA_routeur_simple.T_LCA;
      Res   : Unbounded_String;
      Defauts, Demandes : Integer;
      Taux : Float;
   begin
      Put("Tester_Integration_Hit_Miss : ");
      Cache_Arbre.Initialiser(Cache, 2);
      LCA_routeur_simple.Initialiser(Table);

      LCA_routeur_simple.Enregistrer(Table, 1, (IP_1 and M_24, M_24, To_Unbounded_String("eth0")));

      begin
         Res := Rechercher(Cache, IP_1);
         pragma Assert(False); 
      exception
         when Cle_Absente_Error =>
            declare
               Route : T_Case := Trouver_Route_LPM(Table, IP_1);
            begin
               Res := Route.Int;
               Cache_Arbre.Enregistrer(Cache, Route.Destination, Route.Masque, Route.Int);
            end;
      end;
      pragma Assert(Res = To_Unbounded_String("eth0"));

      Res := Rechercher(Cache, IP_1);
      pragma Assert(Res = To_Unbounded_String("eth0"));

      Obtenir_Statistiques(Cache, Defauts, Demandes, Taux);
      pragma Assert(Demandes = 2);
      pragma Assert(Defauts = 1);

      Detruire(Cache);
      LCA_routeur_simple.Detruire(Table);
      Put_Line("OK");
   end Tester_Integration_Hit_Miss;

   procedure Tester_LPM_Table_Cache is
      Cache : T_Cache;
      Table : LCA_routeur_simple.T_LCA;
      Res   : Unbounded_String;
   begin
      Put("Tester_LPM_Table_Cache : ");
      Cache_Arbre.Initialiser(Cache, 5);
      LCA_routeur_simple.Initialiser(Table);

      LCA_routeur_simple.Enregistrer(Table, 1, (IP_1 and M_24, M_24, To_Unbounded_String("eth0")));
      LCA_routeur_simple.Enregistrer(Table, 2, (IP_1 and M_32, M_32, To_Unbounded_String("eth1")));

      declare
         Route : T_Case := Trouver_Route_LPM(Table, IP_1);
      begin
         pragma Assert(Route.Int = To_Unbounded_String("eth1"));
         Cache_Arbre.Enregistrer(Cache, Route.Destination, Route.Masque, Route.Int);
      end;

      Res := Rechercher(Cache, IP_1);
      pragma Assert(Res = To_Unbounded_String("eth1"));

      Detruire(Cache);
      LCA_routeur_simple.Detruire(Table);
      Put_Line("OK");
   end Tester_LPM_Table_Cache;

   procedure Tester_Eviction_FIFO_Integration is
      Cache : T_Cache;
      Table : LCA_routeur_simple.T_LCA;
   begin
      Put("Tester_Eviction_FIFO_Integration : ");
      Cache_Arbre.Initialiser(Cache, 1);
      LCA_routeur_simple.Initialiser(Table);

      LCA_routeur_simple.Enregistrer(Table, 1, (IP_1 and M_24, M_24, To_Unbounded_String("eth0")));
      LCA_routeur_simple.Enregistrer(Table, 2, (IP_3 and M_8, M_8, To_Unbounded_String("eth2")));

      declare
         Route1 : T_Case := Trouver_Route_LPM(Table, IP_1);
      begin
         Cache_Arbre.Enregistrer(Cache, Route1.Destination, Route1.Masque, Route1.Int);
      end;

      if Cache_Arbre.Taille(Cache) >= 1 then
         Cache_Arbre.Supprimer_FIFO(Cache);
      end if;
      
      declare
         Route2 : T_Case := Trouver_Route_LPM(Table, IP_3);
      begin
         Cache_Arbre.Enregistrer(Cache, Route2.Destination, Route2.Masque, Route2.Int);
      end;

      pragma Assert(Cache_Arbre.Taille(Cache) = 1);
      pragma Assert(Rechercher(Cache, IP_3) = To_Unbounded_String("eth2"));

      begin
         declare
            S : Unbounded_String := Rechercher(Cache, IP_1);
         begin
            pragma Assert(False);
         end;
      exception
         when Cle_Absente_Error => null;
      end;

      Detruire(Cache);
      LCA_routeur_simple.Detruire(Table);
      Put_Line("OK");
   end Tester_Eviction_FIFO_Integration;

begin
   Put_Line("=== Debut des tests integration Routeur_LA ===");
   Tester_Integration_Hit_Miss;
   Tester_LPM_Table_Cache;
   Tester_Eviction_FIFO_Integration;
   Put_Line("=== Tous les tests ont reussi ===");
end Tester_Routeur_LA;