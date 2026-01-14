with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Fonctions_globales;    use Fonctions_globales;
with Cache_Arbre;           use Cache_Arbre;
with SDA_Exceptions;        use SDA_Exceptions;
with Routeur_exceptions;    use Routeur_exceptions;

procedure Routeur_LA is

   Taille_C      : Integer;
   Politique     : Tab_Politique;
   Stat          : Boolean;
   F_Table_Nom   : Unbounded_String;
   F_Paquet_Nom  : Unbounded_String;
   F_Res_Nom     : Unbounded_String;

   Cache         : T_Cache;
   Table         : LCA_routeur_simple.T_LCA;

   F_Paquets     : File_Type;
   F_Resultat    : File_Type;
   F_Table       : File_Type;

   IP_Input      : T_Adresse_IP;
   Interface_R   : Unbounded_String;

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

begin
   Gerer_commandes(Taille_C, Politique, Stat, F_Table_Nom, F_Paquet_Nom, F_Res_Nom);

   -- Initialisation 
   Cache_Arbre.Initialiser(Cache, Taille_C);
   LCA_routeur_simple.Initialiser(Table);

   begin
      Open(F_Table, In_File, To_String(F_Table_Nom));
      declare
         Idx : Integer := 1;
      begin
         while not End_Of_File(F_Table) loop
            declare
               Ligne : String := Get_Line(F_Table);
            begin
               if Ligne'Length > 0 and then Ligne(Ligne'First) /= '#' then
                  declare
                     Parties : array(1..3) of Unbounded_String;
                     Dernier : Integer := Ligne'First;
                     K : Integer := 1;
                  begin
                     for J in Ligne'Range loop
                        if Ligne(J) = ' ' or Ligne(J) = Character'Val(9) then
                           if J > Dernier then
                              Parties(K) := To_Unbounded_String(Ligne(Dernier .. J-1));
                              K := K + 1;
                           end if;
                           Dernier := J + 1;
                        elsif J = Ligne'Last then
                           Parties(K) := To_Unbounded_String(Ligne(Dernier .. J));
                        end if;
                        exit when K > 3;
                     end loop;
                     
                     -- Utilisation du Enregistrer de la LCA
                     LCA_routeur_simple.Enregistrer(Table, Idx, (Id_ad_IP(To_String(Parties(1))), 
                                                                 Id_ad_IP(To_String(Parties(2))), 
                                                                 Parties(3)));
                     Idx := Idx + 1;
                  end;
               end if;
            end;
         end loop;
      end;
      Close(F_Table);
   exception
      when others =>
         Put_Line("Erreur lors du chargement de la table.");
         return;
   end;

   begin
      Open(F_Paquets, In_File, To_String(F_Paquet_Nom));
      Create(F_Resultat, Out_File, To_String(F_Res_Nom));
   exception
      when others =>
         Put_Line("Erreur : Impossible d'ouvrir les fichiers de paquets ou de résultats.");
         return;
   end;

   while not End_Of_File(F_Paquets) loop
      declare
         Ligne : String := Get_Line(F_Paquets);
      begin
         if Ligne'Length > 0 and then Ligne(Ligne'First) /= '#' then
            IP_Input := Id_ad_IP(Ligne);
            
            begin
               Interface_R := Rechercher(Cache, IP_Input);
            exception
               when Cle_Absente_Error =>
                  begin
                     declare
                        Route : T_Case := Trouver_Route_LPM(Table, IP_Input);
                     begin
                        Interface_R := Route.Int;
                        
                        if Taille_C > 0 then
                           if Cache_Arbre.Taille(Cache) >= Taille_C then
                              case Politique is
                                 when FIFO => Supprimer_FIFO(Cache);
                                 when LRU  => Supprimer_LRU(Cache);
                                 when LFU  => Supprimer_LFU(Cache);
                              end case;
                           end if;
                           -- Utilisation du Enregistrer du Cache 
                           Cache_Arbre.Enregistrer(Cache, Route.Destination, Route.Masque, Route.Int);
                        end if;
                     end;
                  exception
                     when Adresse_IP_Introuvable_Error =>
                        Interface_R := To_Unbounded_String("inconnue");
                  end;
            end;

            Put_Line(F_Resultat, To_String(Interface_R));
         end if;
      end;
   end loop;

   if Stat then
      declare
         Defauts, Demandes : Integer;
         Taux : Float;
      begin
         Obtenir_Statistiques(Cache, Defauts, Demandes, Taux);
         Put_Line("Nombre de demandes : " & Integer'Image(Demandes));
         Put_Line("Nombre de defauts  : " & Integer'Image(Defauts));
         Put("Taux de defauts    : ");
         Put(Taux, 1, 4, 0);
         New_Line;
      end;
   end if;

   Close(F_Paquets);
   Close(F_Resultat);
   Detruire(Cache);
   LCA_routeur_simple.Detruire(Table);

exception
   when E : others =>
      Put_Line("Erreur fatale lors de l'execution du routeur.");
end Routeur_LA;